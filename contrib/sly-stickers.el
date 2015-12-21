;;; sly-stickers.el --- Live-code annotations for SLY  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience, languages, lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; There is much in this library that would merit comment. Just some points:
;;;
;;; * Stickers are just overlays that exist on the Emacs side. A lot of the code is just
;;;   for managing overlay priorities and faces so stickers inside stickers are visually
;;;   recognizable.
;;;
;;;   The main entry-point here is the interactive command `sly-sticker-dwim', which
;;;   places and removes stickers.
;;;
;;;   Stickers are also indexed by an integer and placed in a connection-global
;;;   hash-table, `sly-stickers--stickers' It can be connection-global because the same
;;;   sticker with the same id might eventually be sent, multiple times, to many
;;;   connections. It's the Slynk side that has to be able to tell whence the stickers
;;;   comes from (this is not done currently).
;;;
;;; * The gist of stickers is instrumenting top-level forms. This is done by hooking
;;;   onto `sly-compile-region-function'. Two separate compilations are performed: one
;;;   for the uninstrumented form and another for the intrumented form. This is so that
;;;   warnings and compilations errors that are due to stickers exclusively can be
;;;   sorted out. If the second compilation fails, the stickers dont "stick", i.e. they
;;;   are not armed.
;;;
;;; * File compilation is also hooked onto via `sly-compilation-finished-hook'. The idea
;;;   here is to first compile the whole file, then traverse any top-level forms that
;;;   contain stickers and instrument those.
;;;
;;; * On the emacs-side, the sticker overlays are very ephemeral objects. They are not
;;;   persistently saved in any way. Deleting or modifying text inside them
;;;   automatically deletes them.
;;;
;;;   The slynk side eventually must be told to let go of deleted
;;;   stickers. Before this happens these stickers are known as zombies.  Reaping
;;;   happens on almost every SLY -> Slynk call.  Killing the buffer they live in
;;;   doesn't automatically delete them, but reaping eventually happens anyway via
;;;   `sly-stickers--sticker-by-id'.
;;;
;;;   Before a zombie sticker is reaped, some code may still be running that adds
;;;   recordings to these stickers, and some of these recordings make it to the Emacs
;;;   side. The user can ignore them in `sly-stickers--replay', being notified that a
;;;   deleted sticker is being referenced.
;;;
;;;   This need to communicate dead stickers to Slynk is only here because using
;;;   weak-hash-tables is impractical for stickers indexed by integers. Perhaps this
;;;   could be fixed if the instrumented forms could reference sticker objects directly.
;;;
;;; * To see the results of sticker-instrumented code, there are the interactive
;;;   commands `sly-stickers--replay' and `sly-stickers--fetch'. If "breaking stickers"
;;;   is enabled, the debugger is also invoked before a sticker is reached and after a
;;;   sticker returns (if it returns). Auxiliary data-structures like
;;;   `sly-stickers--recording' are used here.
;;;
;;; * `sly-stickers--replay-state' and `sly-stickers--replay-map' are great big hacks
;;;   just for handling the `sly-stickers--replay' interactive loop. Should look into
;;;   recursive minibuffers or something more akin to `ediff', for example.
;;;
;;; Code:


(require 'sly)
(require 'sly-parse "lib/sly-parse")
(require 'sly-buttons "lib/sly-buttons")
(eval-when-compile (require 'cl)) ; Using `cl-defstruct' needs it apparently. See issue
                                  ; https://github.com/capitaomorte/sly/issues/54
(require 'cl-lib)
(require 'hi-lock) ;; for the faces
(require 'color)

(define-sly-contrib sly-stickers
  "Mark expressions in source buffers and annotate return values."
  (:authors "João Távora <joaotavora@gmail.com>")
  (:license "GPL")
  (:slynk-dependencies slynk-stickers)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-stickers-mode)
            (add-hook 'sly-mode-hook 'sly-stickers-shortcut-mode)
            (setq sly-compile-region-function 'sly-stickers-compile-region-aware-of-stickers)
            (add-hook 'sly-compilation-finished-hook 'sly-stickers-after-buffer-compilation t)
            (add-hook 'sly-db-extras-hooks 'sly-stickers--handle-break))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-stickers-mode)
              (remove-hook 'sly-mode-hook 'sly-stickers-shortcut-mode)
              (setq sly-compile-region-function 'sly-compile-region-as-string)
              (remove-hook 'sly-compilation-finished-hook 'sly-stickers-after-buffer-compilation)
              (remove-hook 'sly-db-extras-hooks 'sly-stickers--handle-break)))



;;;; Bookeeping for local stickers
;;;; 
(defvar sly-stickers--counter 0)

(defvar sly-stickers--stickers (make-hash-table))

(defvar sly-stickers--zombie-sticker-ids nil
  "Sticker ids that might exist in Slynk but no longer in Emacs.")

(defun sly-stickers--zombies () sly-stickers--zombie-sticker-ids)

(defun sly-stickers--reset-zombies () (setq sly-stickers--zombie-sticker-ids nil))



;;;; Sticker display and UI logic
;;;; 
(defgroup sly-stickers nil
  "Mark expressions in source buffers and annotate return values."
  :prefix "sly-stickers-"
  :group 'sly)

(when nil
  (cl-loop for sym in '(sly-stickers-placed-face
                        sly-stickers-armed-face
                        sly-stickers-empty-face
                        sly-stickers-recordings-face
                        sly-stickers-exited-non-locally-face)
           do
           (put sym 'face-defface-spec nil)))

(defface sly-stickers-placed-face
  '((((background dark)) (:background "light grey" :foreground "black"))
    (t (:background "light grey")))
  "Face for sticker just set")

(defface sly-stickers-armed-face
  '((t (:strike-through nil :inherit hi-blue)))
  "Face for stickers that have been armed")

(defface sly-stickers-recordings-face
  '((t (:strike-through nil :inherit hi-green)))
  "Face for stickers that have new recordings")

(defface sly-stickers-empty-face
  '((t (:strike-through nil :inherit hi-pink)))
  "Face for stickers that have no recordings.")

(defface sly-stickers-exited-non-locally-face
  '((t (:strike-through t :inherit sly-stickers-empty-face)))
  "Face for stickers that have exited non-locally.")

(defvar sly-stickers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s C-s") 'sly-stickers-dwim)
    (define-key map (kbd "C-c C-s C-d") 'sly-stickers-clear-defun-stickers)
    (define-key map (kbd "C-c C-s C-k") 'sly-stickers-clear-buffer-stickers)
    map))

(defvar sly-stickers-shortcut-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s S") 'sly-stickers-fetch)
    (define-key map (kbd "C-c C-s F") 'sly-stickers-forget)
    (define-key map (kbd "C-c C-s C-r") 'sly-stickers-replay)
    map))

(define-minor-mode sly-stickers-mode
  "Mark expression in source buffers and annotate return values.")

(define-minor-mode sly-stickers-shortcut-mode
  "Shortcuts for navigating sticker recordings.")

(defvar sly-stickers--sticker-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") 'sly-mrepl-copy-part-to-repl)
    (define-key map [down-mouse-3] 'sly-button-popup-part-menu)
    (define-key map [mouse-3] 'sly-button-popup-part-menu)
    map))

(define-button-type 'sly-stickers-sticker :supertype 'sly-part
  'sly-button-inspect 'sly-stickers--inspect-recording
  'sly-button-echo 'sly-stickers--echo-sticker
  'keymap sly-stickers--sticker-map)

(defun sly-stickers--set-tooltip (sticker &optional info)
  (let* ((help-base (button-get sticker 'sly-stickers--base-help-echo))
         (text (if info
                   (concat "[sly] Sticker:" info "\n" help-base)
                 help-base)))
    (button-put sticker 'help-echo text)
    (button-put sticker 'sly-stickers--info info)))

(defun sly-stickers--echo-sticker (sticker &rest more)
  (cl-assert (null more) "Apparently two stickers at exact same location")
  (sly-message (button-get sticker 'sly-stickers--info))
  (sly-button-flash sticker))

(defcustom sly-stickers-max-nested-stickers 4
  "The maximum expected level expected of sticker nesting.
If you nest more than this number of stickers inside other
stickers, the overlay face will be very dark, and probably
render the underlying text unreadable."
  :type :integer)

(defvar sly-stickers-color-face-attribute :background
  "Color-capable attribute of sticker faces that represents nesting.")

(defun sly-stickers--guess-face-color (face)
  (face-attribute-specified-or
   (face-attribute face sly-stickers-color-face-attribute nil t)
   nil))

(defun sly-stickers--set-face (sticker &optional face)
  (let* ((face (or face
                   (button-get sticker 'sly-stickers--base-face)))
         (guessed-color (sly-stickers--guess-face-color face)))
    (button-put sticker 'sly-stickers--base-face face)
    (unless guessed-color
      (sly-error "sorry, can't guess color for face %s for sticker %s"))
    (button-put sticker 'face
                `(:inherit ,face
                           ,sly-stickers-color-face-attribute
                           ,(color-darken-name guessed-color
                                               (* 40
                                                  (/ (sly-button--overlay-priority sticker)
                                                     sly-stickers-max-nested-stickers
                                                     1.0)))))))

(defun sly-stickers--stickers-in (beg end)
  (sly-button--overlays-in beg end 'sly-stickers--sticker-id))
(defun sly-stickers--stickers-at (pos)
  (sly-button--overlays-at pos 'sly-stickers--sticker-id))
(defun sly-stickers--stickers-between (beg end)
  (sly-button--overlays-between beg end 'sly-stickers--sticker-id))
(defun sly-stickers--stickers-exactly-at (beg end)
  (sly-button--overlays-exactly-at beg end 'sly-stickers--sticker-id))


(defun sly-stickers--sticker (from to)
  "Place a new sticker from FROM to TO"
  (let* ((intersecting (sly-stickers--stickers-in from to))
         (contained (sly-stickers--stickers-between from to))
         (not-contained (cl-set-difference intersecting contained))
         (containers nil))
    (unless (cl-every #'(lambda (e)
                          (and (< (button-start e) from)
                               (> (button-end e) to)))
                      not-contained)
      (sly-error "Cannot place a sticker that partially overlaps other stickers"))
    (when (sly-stickers--stickers-exactly-at from to)
      (sly-error "There is already a sticker at those very coordinates"))
    ;; by now we know that other intersecting, non-contained stickers
    ;; are our containers.
    ;; 
    (setq containers not-contained)
    (let* ((label "Brand new sticker")
           (sticker (make-button from to :type 'sly-stickers-sticker
                                 'part-args (list -1 nil)
                                 'part-label label
                                 'sly-button-search-id (sly-button-next-search-id)
                                 'modification-hooks '(sly-stickers--sticker-modified)
                                 'sly-stickers-id (cl-incf sly-stickers--counter)
                                 'sly-stickers--base-help-echo
                                 "mouse-3: Context menu")))
      ;; choose a suitable priorty for ourselves and increase the
      ;; priority of those contained by us
      ;;
      (sly-stickers--set-sticker-piority
       sticker
       (1+ (cl-reduce #'max (mapcar #'sly-button--overlay-priority containers)
                      :initial-value -1)))
      (mapc #'sly-stickers--increase-prio contained)
      ;; finally, set face
      ;;
      (sly-stickers--set-tooltip sticker label)
      (sly-stickers--set-face sticker 'sly-stickers-placed-face)
      sticker)))

(defun sly-stickers--sticker-id (sticker)
  (button-get sticker 'sly-stickers-id))

(defun sly-stickers--arm-sticker (sticker)
  (let* ((id (sly-stickers--sticker-id sticker))
         (label (format "Sticker %d is armed" id)))
    (button-put sticker 'part-args (list id nil))
    (button-put sticker 'part-label label)
    (button-put sticker 'sly-stickers--last-known-recording nil)
    (sly-stickers--set-tooltip sticker label)
    (sly-stickers--set-face sticker 'sly-stickers-armed-face)
    (puthash id sticker sly-stickers--stickers)))

(defun sly-stickers--disarm-sticker (sticker)
  (let* ((id (sly-stickers--sticker-id sticker))
         (label (format "Sticker %d failed to stick" id)))
    (button-put sticker 'part-args (list -1 nil))
    (button-put sticker 'part-label label)
    (sly-stickers--set-tooltip sticker label)
    (sly-stickers--set-face sticker 'sly-stickers-placed-face)))

(define-button-type 'sly-stickers--recording-part :supertype 'sly-part
  'sly-button-inspect
  'sly-stickers--inspect-recording
  ;; 'sly-button-pretty-print
  ;; #'(lambda (id) ...)
  ;; 'sly-button-describe
  ;; #'(lambda (id) ...)
  ;; 'sly-button-show-source
  ;; #'(lambda (id) ...)
  )

(defun sly-stickers--recording-part (label sticker-id recording vindex &rest props)
  (apply #'make-text-button
         label nil
         :type 'sly-stickers--recording-part
         'part-args (list sticker-id recording vindex)
         'part-label "Recorded value"
         props)
  label)

(cl-defun sly-stickers--describe-recording-values (recording &key
                                                             (indent 0)
                                                             (prefix "=> "))
  (cl-flet ((indent (str)
                    (concat (make-string indent ? )str))
            (prefix (str)
                    (concat prefix str)))
    (let ((descs (sly-stickers--recording-value-descriptions recording)))
      (cond ((sly-stickers--recording-exited-non-locally-p recording)
             (indent (propertize "exited non locally" 'face 'sly-action-face)))
            ((null descs)
             (indent (propertize "no values" 'face 'sly-action-face)))
            (t
             (cl-loop for (value-desc . rest) on descs
                      for vindex from 0
                      concat
                      (indent (prefix
                               (sly-stickers--recording-part
                                value-desc
                                (sly-stickers--recording-sticker-id recording)
                                recording
                                vindex)))
                      when rest
                      concat "\n"))))))

(defconst sly-stickers--newline "\n"
  "Work around bug #63, actually Emacs bug #21839.
\"25.0.50; can't use newlines in defaults in cl functions\"")

(cl-defun sly-stickers--pretty-describe-recording (recording &key (separator sly-stickers--newline))
          (let* ((recording-sticker-id (sly-stickers--recording-sticker-id recording))
                 (sticker (gethash recording-sticker-id
                                   sly-stickers--stickers))
                 (nvalues (length (sly-stickers--recording-value-descriptions recording))))
            (format "%s%s:%s%s"
                    (if sticker
                        (format "Sticker %s in line %s of %s"
                                (sly-stickers--sticker-id sticker)
                                (line-number-at-pos (overlay-start sticker))
                                (overlay-buffer sticker))
                      (format "Deleted or unknown sticker %s"
                              recording-sticker-id))
                    (if (cl-plusp nvalues)
                        (format " returned %s values" nvalues) "")
                    separator
                    (sly-stickers--describe-recording-values recording
                                                             :indent 2))))

(defun sly-stickers--populate-sticker (sticker recording)
  (let* ((id (sly-stickers--sticker-id sticker))
         (total (sly-stickers--recording-sticker-total recording)))
    (cond ((cl-plusp total)
           (button-put sticker 'part-label (format "Sticker %d has %d recordings" id total))
           (unless (sly-stickers--recording-void-p recording)
             (button-put sticker 'sly-stickers--last-known-recording recording)
             (button-put sticker 'part-args (list id recording))
             (sly-stickers--set-tooltip sticker
                                        (format "Newest of %s sticker recordings:\n%s"
                                                total
                                                (sly-stickers--describe-recording-values recording :prefix "")))
             (sly-stickers--set-face sticker
                                     (if (sly-stickers--recording-exited-non-locally-p recording)
                                         'sly-stickers-exited-non-locally-face
                                       'sly-stickers-recordings-face))))
          (t
           (let ((last-known-recording (button-get sticker 'sly-stickers--last-known-recording)))
             (button-put sticker 'part-label (format "Sticker %d has no recordings" id))
             (when last-known-recording
               (sly-stickers--set-tooltip sticker
                                          (format "No new recordings. Last known:\n%s"
                                                  (sly-stickers--describe-recording-values last-known-recording))))
             (sly-stickers--set-tooltip sticker "No new recordings")
             (sly-stickers--set-face sticker 'sly-stickers-empty-face))))))

(defun sly-stickers--sticker-substickers (sticker)
  (let* ((retval
          (remove sticker
                  (sly-stickers--stickers-between (button-start sticker) (button-end sticker))))
         ;; To verify an important invariant, and warn (don't crash)
         ;; 
         (exactly-at
          (sly-stickers--stickers-exactly-at (button-start sticker) (button-end sticker))))
    (cond ((remove sticker exactly-at)
           (sly-warning "Something's fishy. More than one sticker at same position")
           (cl-set-difference retval exactly-at))
          (t
           retval))))

(defun sly-stickers--briefly-describe-sticker (sticker)
  (let ((beg (button-start sticker))
        (end (button-end sticker)))
    (if (< (- end beg) 20)
        (format "sticker around %s" (buffer-substring-no-properties beg end))
      (cl-labels ((word (point direction)
                        (apply #'buffer-substring-no-properties
                               (sort (list
                                      point
                                      (save-excursion (goto-char point)
                                                      (forward-word direction)
                                                      (point)))
                                     #'<))))
        (format "sticker from \"%s...\" to \"...%s\""
                (word beg 1)
                (word end -1))))))

(defun sly-stickers--set-sticker-piority (sticker prio)
  (overlay-put sticker 'priority prio))

(defun sly-stickers--decrease-prio (sticker)
  (let ((prio (sly-button--overlay-priority sticker)))
    (unless (and prio
                 (cl-plusp prio))
      (sly-error "Something's fishy with the sticker priorities"))
    (sly-stickers--set-sticker-piority sticker (cl-decf prio))
    (sly-stickers--set-face sticker)))

(defun sly-stickers--increase-prio (sticker)
  (let ((prio (sly-button--overlay-priority sticker)))
    (sly-stickers--set-sticker-piority sticker (cl-incf prio))
    (sly-stickers--set-face sticker)))

(defun sly-stickers--delete (sticker)
  "Ensure that sticker is deleted."
  ;; Delete the overlay and take care of priorities for contained and
  ;; containers, but note that a sticker might have no buffer anymore
  ;; if that buffer was killed, for example...
  ;; 
  (when (and (overlay-buffer sticker)
             (buffer-live-p (overlay-buffer sticker)))
    (mapc #'sly-stickers--decrease-prio
          (sly-stickers--sticker-substickers sticker))
    (delete-overlay sticker))
  ;; We also want to deregister it from the hashtable in case it's
  ;; there (it's not there if it has never been armed)
  ;; 
  (let ((id (sly-stickers--sticker-id sticker)))
    (when (gethash (sly-stickers--sticker-id sticker)
                   sly-stickers--stickers)
      (remhash id sly-stickers--stickers)
      (add-to-list 'sly-stickers--zombie-sticker-ids id))))

(defun sly-stickers--sticker-modified (sticker _after? beg end &optional _pre-change-len)
  (unless (save-excursion
            (goto-char beg)
            (skip-chars-forward "\t\n\s")
            (>= (point) end))
    (let ((inhibit-modification-hooks t))
      (sly-message "Deleting %s" (sly-stickers--briefly-describe-sticker sticker))
      (sly-stickers--delete sticker))))

(defun sly-stickers-next-sticker (&optional n)
  (interactive "p")
  (sly-button-search n 'sly-stickers--sticker-id))

(defun sly-stickers-prev-sticker (&optional n)
  (interactive "p")
  (sly-button-search (- n) 'sly-stickers--sticker-id))

(defun sly-stickers-clear-defun-stickers ()
  "Clear all stickers in the current top-level form."
  (interactive)
  (let* ((region (sly-region-for-defun-at-point)))
    (sly-stickers-clear-region-stickers (car region) (cadr region))))

(defun sly-stickers-clear-buffer-stickers ()
  "Clear all the stickers in the current buffer."
  (interactive)
  (sly-stickers-clear-region-stickers (point-min) (point-max)))

(defun sly-stickers-clear-region-stickers (&optional from to)
  "Clear all the stickers between FROM and TO."
  (interactive "r")
  (let* ((from (or from (region-beginning)))
         (to (or to (region-end)))
         (stickers (sly-stickers--stickers-in from to)))
    (cond (stickers
           (mapc #'sly-stickers--delete stickers)
           (sly-message "%s stickers cleared" (length stickers)))
          (t
           (sly-message "no stickers to clear")))))

(defun sly-stickers-delete-sticker-at-point (&optional point)
  "Delete the topmost sticker at point."
  (interactive "d")
  (let ((stickers (sly-stickers--stickers-at (or point (point)))))
    (cond (stickers
           (sly-stickers--delete (car stickers))
           (if (cdr stickers)
               (sly-message "Deleted topmost sticker (%d remain at point)"
                            (length (cdr stickers)))
             (sly-message "Deleted sticker %s"
                          (sly-stickers--briefly-describe-sticker (car stickers)))))
          (t
           (sly-error "No stickers at point")))))

(defun sly-stickers-maybe-add-sticker (&optional point)
  "Add of remove a sticker at POINT.
If point is currently at a sticker boundary, delete that sticker,
otherwise, add a sticker to the sexp at point."
  (interactive "d")
  (save-excursion
    (goto-char (or point (point)))
    (let* ((bounds (sly-bounds-of-sexp-at-point))
           (beg (car bounds))
           (end (cdr bounds))
           (matching (and bounds
                          (sly-stickers--stickers-exactly-at beg end))))
      (cond ((not bounds)
             (sly-message "Nothing here to place sticker on, apparently"))
            (matching
             (sly-stickers--delete (car matching))
             (sly-message "Deleted sticker"))
            (t
             (let ((sticker (sly-stickers--sticker beg end)))
               (sly-message "Added %s" (sly-stickers--briefly-describe-sticker sticker))))))))

(defun sly-stickers-dwim (prefix)
  "Set or remove stickers at point.
Set a sticker for the current sexp at point, or delete it if it
already exists.

If the region is active set a sticker in the current region.

With interactive prefix arg PREFIX always delete stickers.

- One C-u means delete the current top-level form's stickers.
- Two C-u's means delete the current buffer's stickers"
  (interactive "p") 
  (cond
   ((= prefix 4)
    (if (region-active-p)
        (sly-stickers-clear-region-stickers)
      (sly-stickers-clear-defun-stickers)))
   ((>= prefix 16)
    (sly-stickers-clear-buffer-stickers))
   ((region-active-p)
    (sly-stickers--sticker (region-beginning) (region-end))
    (deactivate-mark t))
   ((not (sly-inside-string-or-comment-p))
    (sly-stickers-maybe-add-sticker))
   (t
    (sly-message "No point placing stickers in string literals or comments"))))

(defun sly-stickers--sticker-by-id (sticker-id)
  "Return the sticker for STICKER-ID, or return NIL.
Perform some housecleaning tasks for stickers that have been
properly deleted or brutally killed with the buffer they were in."
  (let* ((sticker (gethash sticker-id sly-stickers--stickers)))
    (cond ((and sticker (overlay-buffer sticker)
                (buffer-live-p (overlay-buffer sticker)))
           sticker)
          (sticker
           ;; `sticker-id' references a sticker that hasn't been
           ;; deleted but whose overlay can't be found. One reason for
           ;; this is that the buffer it existed in was killed. So
           ;; delete it now and mark it a zombie.
           (sly-stickers--delete sticker)
           nil)
          (t
           ;; The sticker isn't in the `sly-stickers--stickers' hash
           ;; table, so it has probably already been marked zombie,
           ;; and possibly already deleted. We're probably just seeing
           ;; it because recording playback and breaking stickers may
           ;; not filtering these out by user option.
           ;;
           ;; To be on the safe side, add the id to the table anyway,
           ;; so it'll get killed on the Slynk side on the next
           ;; request.
           ;; 
           (add-to-list 'sly-stickers--zombie-sticker-ids sticker-id)
           nil))))

(cl-defun sly-stickers--find-and-flash (sticker-id &key (otherwise nil))
  "Find and flash the sticker referenced by STICKER-ID.
otherwise call OTHERWISE with a single argument, a string stating
the reason why the sticker couldn't be found"
  (let ((sticker (sly-stickers--sticker-by-id sticker-id)))
    (cond (sticker
           (let ((buffer (overlay-buffer sticker)))
             (when buffer
               (with-current-buffer buffer
                 (let ((window (display-buffer buffer)))
                   (let ((orig (point)))
                     (goto-char (overlay-start sticker))
                     (when window
                       (with-selected-window window
                         (sly-recenter orig)
                         (sly-button-flash sticker
                                           :timeout 0.2 :times 4
                                           :face 'highlight)))))))))
          (otherwise
           (funcall otherwise "Can't find sticker (probably deleted!)")))))


;;;; Recordings
;;;; 
(cl-defstruct (sly-stickers--recording
               (:constructor sly-stickers--make-recording-1)
               (:conc-name sly-stickers--recording-)
               (:copier sly-stickers--copy-recording))
  (sticker-id nil)
  (sticker-total nil)
  (id nil)
  (value-descriptions nil)
  (exited-non-locally-p nil)
  (sly-connection nil))

(defun sly-stickers--recording-void-p (recording)
  (not (sly-stickers--recording-id recording)))

(defun sly-stickers--make-recording (description)
  "Make a `sly-stickers--recording' from DESCRIPTION.
A DESCRIPTION is how the Lisp side describes a sticker and
usually its most recent recording. If it doesn't, a recording
veryfying `sly-stickers--recording-void-p' is created."
  (cl-destructuring-bind (sticker-id sticker-total . recording-description)
      description
    (let ((recording (sly-stickers--make-recording-1
                      :sticker-id sticker-id
                      :sticker-total sticker-total
                      :sly-connection (sly-current-connection))))
      (when recording-description
        (cl-destructuring-bind (recording-id value-descriptions exited-non-locally-p)
            recording-description
          (setf
           (sly-stickers--recording-id recording) recording-id
           (sly-stickers--recording-value-descriptions recording) value-descriptions
           (sly-stickers--recording-exited-non-locally-p recording) exited-non-locally-p)))
      recording)))


;;;; Replaying sticker recordings
;;;; 
(defvar sly-stickers--replay-map nil)
(defvar sly-stickers--replay-help nil)

(cl-flet ((def
           (key binding &optional desc)
           (define-key sly-stickers--replay-map (kbd key) binding)
           (setf (cl-getf sly-stickers--replay-help binding)
                 (cons (cons key (car (cl-getf sly-stickers--replay-help binding)))
                       (or desc
                           (cdr (cl-getf sly-stickers--replay-help binding)))))))
  (setq sly-stickers--replay-map (make-sparse-keymap)
        sly-stickers--replay-help '())
  
  (def "n" :next "Scan recordings forward")
  (def "SPC" :next)
  (def "DEL" :prev "Scan recordings backward")
  (def "p" :prev)
  (def "i" 'sly-stickers--inspect-recording "Inspect first sticker value")
  (def "M-RET" 'sly-stickers--copy-recording-to-repl "Return sticker values to REPL")
  (def "j" 'jump "Jump to a recording")
  (def "J" 'jump-to-newest "Jump to newest recordings")
  (def ">" 'end "Go to last recording")
  (def "<" 'beginning "Go to last recording")
  (def "h" 'sly-stickers--toggle-help "Toggle help")
  (def "C-h" 'sly-stickers--toggle-help)
  (def "q" 'quit "Quit")
  (def "C-g" 'quit)
  (def "x" 'ignore-sticker "Ignore this sticker")
  (def "z" 'ignore-zombies "Toggle ignoring deleted stickers")
  (def "R" 'reset-ignored-stickers "Reset ignore list"))


(defvar sly-stickers--expanded-help t)

(defun sly-stickers--toggle-help ()
  (interactive)
  (setq sly-stickers--expanded-help (not sly-stickers--expanded-help)))

(cl-defstruct (sly-stickers--replay-state
               (:constructor sly-stickers--make-state)
               (:conc-name sly-stickers--state-)
               (:copier sly-stickers--copy-state))
  (key (cl-gensym "sticker-visitor-"))
  (binding 0)
  (ignored-stickers (cons t '()))
  (error nil)
  (total 0)
  (recording nil))

(sly-def-connection-var sly-stickers--replay-last-state nil
  "State last left off by `sly-stickers-replay' for this connection")

(defun sly-stickers--replay-prompt (state)
  "Produce a prompt and status string for STATE."
  (let ((recording (sly-stickers--state-recording state))
        (ignored-stickers (sly-stickers--state-ignored-stickers state)))
    (cl-labels
        ((paragraph () (if sly-stickers--expanded-help "\n\n" "\n"))
         (describe-ignored-stickers
          ()
          (let ((ignored-ids (cdr ignored-stickers))
                (ignore-zombies (car ignored-stickers)))
            (if (or ignored-ids ignore-zombies)
                (format "%s%s%s"
                        (paragraph)
                        (if (car ignored-stickers) "Skipping recordings of deleted stickers. " "")
                        (if ignored-ids
                            (format "Skipping recordings of sticker%s %s."
                                    (if (cadr (cdr ignored-stickers)) "s" "")
                                    (concat (mapconcat #'pp-to-string
                                                       (butlast (cdr ignored-stickers))
                                                       ", ")
                                            (and (cadr (cdr ignored-stickers)) " and ")
                                            (pp-to-string
                                             (car (last (cdr ignored-stickers))))))
                          ""))
              "")))
         (describe-help
          ()
          (format "%s%s"
                  (paragraph)
                  (if sly-stickers--expanded-help
                      (cl-loop with rows =
                               (loop for (_binding spec) on sly-stickers--replay-help by #'cddr
                                     for (keys . desc) = spec
                                     collect (cons (mapconcat #'identity (reverse keys) ", ")
                                                   desc))
                               with (maxkey maxdesc) = (cl-loop for (key . desc) in rows
                                                                maximize (length key) into maxkey
                                                                maximize (length desc) into maxdesc
                                                                finally (cl-return (list maxkey maxdesc)))
                               for (odd even) on (reverse rows) by #'cddr
                               for (odd-key . odd-desc) = odd
                               for (even-key . even-desc) = (or even '("" . ""))
                               concat (truncate-string-to-width odd-key maxkey 0 ? )
                               concat "   "
                               concat (truncate-string-to-width odd-desc maxdesc 0 ? )
                               concat "   |   "
                               concat (truncate-string-to-width even-key maxkey 0 ? )
                               concat "   "
                               concat (truncate-string-to-width even-desc maxdesc 0 ? )
                               concat "\n")
                    "(n)ext)/(p)revious/(i)gnore/h(elp)/(q)uit")))
         (describe-playhead
          ()
          (let* ((new-total (sly-stickers--state-total state))
                 (old-total (and (sly-stickers--replay-last-state)
                                (sly-stickers--state-total (sly-stickers--replay-last-state))))
                 (diff (and old-total (- new-total old-total))))
           (format "Replaying recording %s of %s%s"
                  (1+ (sly-stickers--recording-id recording))
                  (sly-stickers--state-total state)
                  (cond ((and diff
                              (cl-plusp diff))
                         (format ", %s new since last replay"
                                 diff))
                        (old-total
                         ", no new recordings")
                        (t
                         ""))))))
      (concat
              (describe-playhead)
              (paragraph)
              (sly-stickers--pretty-describe-recording recording :separator (paragraph))
              (if ignored-stickers (describe-ignored-stickers) "")
              (describe-help)))))

(defun sly-stickers--replay-read-binding (state)
  "Read a binding from the user and modify STATE."
  (let ((max-mini-window-height 0.99)
        (prompt (sly-stickers--replay-prompt state))
        (recording (sly-stickers--state-recording state)))
    (setf (sly-stickers--state-binding state)
          (lookup-key sly-stickers--replay-map (vector (read-key prompt)) t))
    (let* ((binding (sly-stickers--state-binding state))
           (ignored-stickers (sly-stickers--state-ignored-stickers state))
           (current-sticker-id (sly-stickers--recording-sticker-id recording)))
      (cond ((and (symbolp binding)
                  (fboundp binding)
                  (string-match "^sly-" (symbol-name binding)))
             (if (commandp binding)
                 (call-interactively binding)
               (apply binding
                      (list current-sticker-id recording))
               (signal 'quit nil)))
            ((eq binding 'ignore-sticker)
             (setcdr ignored-stickers
                     (delete-dups
                      (cons (sly-stickers--recording-sticker-id recording)
                            (cdr ignored-stickers)))))
            ((eq binding 'ignore-zombies)
             (setcar ignored-stickers (not (car ignored-stickers))))
            ((eq binding 'reset-ignored-stickers)
             (setcar ignored-stickers t)
             (setcdr ignored-stickers nil))
            ((eq binding 'jump)
             (let ((read (read-number (format "Jump to which recording [1-%d]?"
                                              (sly-stickers--state-total state)))))
               (setq binding (1- (round read)))))
            ((eq binding 'jump-to-newest)
             (let* ((new-total (sly-stickers--state-total state))
                    (old-total (and (sly-stickers--replay-last-state)
                                    (sly-stickers--state-total (sly-stickers--replay-last-state))))
                    (diff (- new-total old-total)))
               (if (cl-plusp diff)
                   (setq binding (1- old-total))
                 (error "No new recordings"))))
            ((eq binding 'beginning)
             (setq binding 0))
            ((eq binding 'end)
             (setq binding (1- (sly-stickers--state-total state))))
            ((null binding)
             (error "Invalid key. You can quit this sticker replay with `q' or `C-g'")))
      (setf (sly-stickers--state-binding state) binding))
    state))

(defun sly-stickers--replay-starting-state ()
  (sly-stickers--make-state))

(defun sly-stickers--replay-fetch-next (state)
  "Update STATE from Slynk according to its bindings."
  (let ((result (sly-eval
                 `(slynk-stickers:search-for-recording
                   ',(sly-stickers--state-key state)
                   ',(sly-stickers--state-ignored-stickers state)
                   ',(sly-stickers--zombies)  
                   ,(sly-stickers--state-binding state)))))
    (sly-stickers--reset-zombies)
    (cond ((car result)
           (setf (sly-stickers--state-error state) nil)
           (cl-destructuring-bind (total . sticker-description)
               result
             (setf (sly-stickers--state-recording state)
                   (sly-stickers--make-recording sticker-description))
             (setf (sly-stickers--state-total state) total)
             ;; Assert that the recording isn't void
             ;; 
             (when (sly-stickers--recording-void-p
                    (sly-stickers--state-recording state))
               (sly-error "Attempt to visit a void recording described by %s"
                          sticker-description))))
          (t
           (setf (sly-stickers--state-error state) (cadr result))))
    state))


(defun sly-stickers--replay-quit-state-p (state)
  (or
   (not (sly-stickers--state-recording state))
   (eq (sly-stickers--state-binding state) 'quit)))

(defun sly-stickers-replay ()
  "Interactively replay sticker recordings fetched from Slynk.
See also `sly-stickers-fetch'."
  (interactive)
  (let ((state (sly-stickers--make-state))
        (last-recording (and (sly-stickers--replay-last-state)
                             (sly-stickers--state-recording
                              (sly-stickers--replay-last-state)))))
    (when last-recording
      (setf (sly-stickers--state-recording state) last-recording)
      (setf (sly-stickers--state-binding state) (sly-stickers--recording-id last-recording)))
    (unwind-protect
        (cl-loop with sly-stickers--expanded-help = t
                 for next-state = (if (or (memq (sly-stickers--state-binding state)
                                                '(:next :prev))
                                          (numberp (sly-stickers--state-binding state)))
                                      (sly-stickers--replay-fetch-next state)
                                    state)
                 while (not (sly-stickers--replay-quit-state-p state))
                 do
                 ;; Pop to the sticker buffer and try to flash it
                 ;;
                 (sly-stickers--find-and-flash
                  (sly-stickers--recording-sticker-id
                                 (sly-stickers--state-recording next-state)))
                 (condition-case err
                     (setq state
                           (sly-stickers--replay-read-binding next-state))
                   (quit
                    err
                    (setf (sly-stickers--state-binding state)
                          'quit))
                   (error
                    (sit-for 0.01)
                    (sly-message (cl-second err))
                    (sit-for 1.5)))
                 (sit-for 0.01)
                 until (sly-stickers--replay-quit-state-p state))
      (cond ((sly-stickers--state-recording state)
             (setf (sly-stickers--replay-last-state) state)
             (sly-message
              (substitute-command-keys "Quit sticker recording replay. You can resume with \\[sly-stickers-replay]")))
            (t
             (sly-message "No sticker recordings. Run some sticker-aware code first."))))))

(defun sly-stickers-fetch ()
  "Fetch recordings from Slynk and update stickers accordingly.
See also `sly-stickers-replay'."
  (interactive)
  (sly-eval-async `(slynk-stickers:fetch ',(sly-stickers--zombies))
    #'(lambda (result)
        (sly-stickers--reset-zombies)
        (let ((message (format "Fetched recordings for %s armed stickers" (length result))))
          (cl-loop for sticker-description in result
                   ;; Although we are analysing sticker descriptions
                   ;; here, recordings are made to pass to
                   ;; `sly-stickers--sticker-by-id', even if they are
                   ;; are `sly-stickers--recording-void-p', which is
                   ;; the case if the sticker has never been
                   ;; traversed.
                   ;; 
                   for recording = (sly-stickers--make-recording sticker-description)
                   for sticker = (sly-stickers--sticker-by-id
                                  (sly-stickers--recording-sticker-id recording))
                   when sticker
                   do (sly-stickers--populate-sticker sticker recording))
          (sly-message message)))
    "CL_USER"))

(defun sly-stickers-forget ()
  "Forget about sticker recordings in the Slynk side."
  (interactive)
  (when (yes-or-no-p "[sly] Really forget about sticker recordings?")
    (sly-eval-async `(slynk-stickers:forget ',(sly-stickers--zombies))
      #'(lambda (_result)
          (sly-stickers--reset-zombies)
          (setf (sly-stickers--replay-last-state) nil)
          (sly-message "Forgot all about sticker recordings.")))))


;;;; Breaking stickers
(defun sly-stickers--handle-break (extra)
  (sly-dcase extra
    ((:slynk-after-sticker description)
     (let ((sticker-id (first description))
           (recording (sly-stickers--make-recording description)))
       (sly-stickers--find-and-flash sticker-id
                                     :otherwise 'sly-message)
       (insert
        "\n\n"
        (sly-stickers--pretty-describe-recording recording
                                                 ))))
    ((:slynk-before-sticker sticker-id)
     (sly-stickers--find-and-flash sticker-id
                                   :otherwise 'sly-message))
    (;; don't do anything if we don't know this "extra" info
     t
     nil)))


(defun sly-stickers-toggle-break-on-stickers ()
  (interactive)
  (let ((break-p (sly-eval '(slynk-stickers:toggle-break-on-stickers))))
    (sly-message "Breaking on stickers is %s" (if break-p "ON" "OFF"))))


;;;; Functions for examining recordings
;;;;


(eval-after-load "sly-mrepl"
  `(progn
     (button-type-put 'sly-stickers-sticker
                      'sly-mrepl-copy-part-to-repl
                      'sly-stickers--copy-recording-to-repl)
     (button-type-put 'sly-stickers--recording-part
                      'sly-mrepl-copy-part-to-repl
                      'sly-stickers--copy-recording-to-repl)))


;;; shoosh byte-compiler
(declare-function sly-mrepl--save-and-copy-for-repl nil)
(cl-defun sly-stickers--copy-recording-to-repl (_sticker-id recording
                                                                 &optional (vindex 0))
       (check-recording recording)
       (sly-mrepl--save-and-copy-for-repl
        `(slynk-stickers:find-recording-or-lose
          ,(sly-stickers--recording-id recording)
          ,vindex)
        :before (format "Returning values of recording %s of sticker %s"
                        (sly-stickers--recording-id recording)
                        (sly-stickers--recording-sticker-id recording))))

(defun check-recording (recording)
  (cond ((null recording)
         (sly-error "This sticker doesn't seem to have any recordings"))
        ((not (eq (sly-stickers--recording-sly-connection recording)
                 (sly-current-connection)))
         (sly-error "Recording is for a different connection (%s)"
                    (sly-connection-name 
                     (sly-stickers--recording-sly-connection recording))))))

(cl-defun sly-stickers--inspect-recording (_sticker-id recording &optional (vindex 0))
  (check-recording recording)
  (sly-eval-for-inspector
   `(slynk-stickers:inspect-sticker-recording ,(sly-stickers--recording-id recording)
                                              ,vindex)))

;;;; Sticker-aware compilation
;;;; 

(cl-defun sly-stickers--compile-region-aware-of-stickers-1
    (start end callback &key sync fallback flash)
  "Compile from START to END considering stickers.
After compilation call CALLBACK with the stickers and the
compilation result.  If SYNC, use `sly-eval' other wise use
`sly-eval-async'.  If FALLBACK, send the uninstrumneted region as
a fallback.  If FLASH, flash the compiled region."
  (let* ((uninstrumented (buffer-substring-no-properties start end))
         (stickers (sly-stickers--stickers-between start end))
         (original-buffer (current-buffer)))
    (cond (stickers
           (when flash
             (sly-flash-region start end :face 'sly-stickers-armed-face))
           (sly-with-popup-buffer ((sly-buffer-name :stickers :hidden t)
                                   :select :hidden)
             (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
             (insert uninstrumented)
             ;; Use a second set of overlays placed just in the
             ;; pre-compilation buffer. We need this to correctly keep
             ;; track of the markers because in this buffer we are going
             ;; to change actual text
             ;; 
             (cl-loop for sticker in stickers
                      for overlay = (make-overlay (- (button-start sticker) (1- start))
                                                  (- (button-end sticker) (1- start)))
                      do (overlay-put overlay 'sly-stickers--sticker sticker))
             (cl-loop for overlay in (overlays-in (point-min) (point-max))
                      for sticker = (overlay-get overlay 'sly-stickers--sticker)
                      do
                      (sly-stickers--arm-sticker sticker)
                      (goto-char (overlay-start overlay))
                      (insert (format "(slynk-stickers:record %d " (sly-stickers--sticker-id sticker)))
                      (goto-char (overlay-end overlay))
                      (insert ")"))
             ;; Now send both the instrumented and uninstrumented
             ;; string to the Lisp
             ;;
             (let ((instrumented (buffer-substring-no-properties (point-min) (point-max)))
                   (new-ids (mapcar #'sly-stickers--sticker-id stickers)))
               (with-current-buffer original-buffer
                 (let ((form `(slynk-stickers:compile-for-stickers
                               ',new-ids
                               ',(sly-stickers--zombies)
                               ,instrumented
                               ,(when fallback uninstrumented)
                               ,(buffer-name)
                               ',(sly-compilation-position start)
                               ,(if (buffer-file-name) (sly-to-lisp-filename (buffer-file-name)))
                               ',sly-compilation-policy)))
                   (cond (sync
                          (funcall callback
                                   stickers
                                   (sly-eval form))
                          (sly-stickers--reset-zombies))
                         (t (sly-eval-async form
                              (lambda (result)
                                (sly-stickers--reset-zombies)
                                (funcall callback stickers result))))))))))
          (t
           (sly-compile-region-as-string start end)))))

(defun sly-stickers-compile-region-aware-of-stickers (start end)
  "Compile region from START to END aware of stickers.
Intended to be placed in `sly-compile-region-function'"
  (sly-stickers--compile-region-aware-of-stickers-1
   start end
   (lambda (stickers result-and-stuck-p)
     (cl-destructuring-bind (result &optional stuck-p)
         result-and-stuck-p
       (unless stuck-p
         (mapc #'sly-stickers--disarm-sticker stickers))
       (sly-compilation-finished
        result
        nil
        (if stuck-p
            (format " (%d stickers armed)" (length stickers))
          " (stickers failed to stick)"))))
   :fallback t
   :flash t))

(defun sly-stickers-after-buffer-compilation (success _notes buffer loadp)
  "After compilation, compile regions with stickers.
Intented to be placed in `sly-compilation-finished-hook'"
  (when (and buffer loadp success)
    (save-restriction
      (widen)
      (let* ((all-stickers (sly-stickers--stickers-between (point-min) (point-max)))
             (regions (cl-loop for sticker in all-stickers
                               for region = (sly-region-for-defun-at-point (overlay-start sticker))
                               unless (member region regions)
                               collect region into regions
                               finally (cl-return regions))))
        (when regions
          (cl-loop with successful
                   with unsuccessful
                   for region in regions
                   do
                   (sly-stickers--compile-region-aware-of-stickers-1
                    (car region) (cadr region)
                    (lambda (stickers result)
                      (cond (result
                             (push (cons region stickers) successful))
                            (t
                             (mapc #'sly-stickers--disarm-sticker stickers)
                             (push (cons region stickers) unsuccessful))))
                    :sync t)
                   finally
                   (sly-temp-message
                    3 3
                    "%s stickers stuck in %s regions, %s disarmed in %s regions"
                    (cl-reduce #'+ successful :key (lambda (x) (length (cdr x))))
                    (length successful)
                    (cl-reduce #'+ unsuccessful :key (lambda (x) (length (cdr x))))
                    (length unsuccessful))))))))


;;;; Menu
;;;;

(easy-menu-define sly-stickers--shortcut-menu nil
  "Placing stickers in `lisp-mode' buffers."
  (let* ((in-source-file 'sly-stickers-mode)
         (connected '(sly-connected-p)))
    `("Stickers"
      ["Add or remove sticker at point" sly-stickers-dwim ,in-source-file]
      ["Delete stickers from top-level form" sly-stickers-clear-defun-stickers ,in-source-file]
      ["Delete stickers from buffer" sly-stickers-clear-buffer-stickers ,in-source-file]
      "--"
      ["Start sticker recording replay" sly-stickers-replay ,connected]
      ["Fetch most recent recordings" sly-stickers-fetch ,connected]
      ["Toggle breaking on stickers" sly-stickers-toggle-break-on-stickers ,connected])))

(easy-menu-add-item sly-menu nil sly-stickers--shortcut-menu "Documentation")

(provide 'sly-stickers)
;;; sly-stickers.el ends here

