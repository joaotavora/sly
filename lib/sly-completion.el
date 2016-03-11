;;; sly-completion.el --- completion tricks and helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  João Távora

;; Author: João Távora
;; Keywords: 

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

;; 

;;; Code:
;;;
(require 'cl-lib)
(require 'comint)
(require 'sly-messages "lib/sly-messages")


;;; Forward declarations (later replace with a `sly-common' lib)
;;; 
(defvar sly-current-thread)

(declare-function sly-eval "sly" '(sexp &optional package))

(declare-function sly-symbol-at-point "sly")

(declare-function sly-buffer-name "sly")

(defvar sly-buffer-package)

(defvar sly-buffer-connection)

(declare-function sly-connection "sly")

(declare-function sly-recenter "sly")

(declare-function sly-symbol-start-pos "sly")

(declare-function sly-symbol-end-pos "sly")

(declare-function sly-current-package "sly")

(declare-function with-displayed-buffer-window "window")


;;; Customization
;;;
(defcustom sly-complete-symbol-function 'sly-flex-completions
  "Function reponsible for SLY completion.
When called with one argument, a pattern, returns a (possibly
propertized) list of strings the complete that pattern,
collected from the Slynk server."
  :type 'function
  :group 'sly-ui)


;;; Completion calculation
;;;
(defun sly--completion-request-completions (pattern slyfun)
  (let ((sly-current-thread t))
    (sly-eval
     `(,slyfun ,(substring-no-properties pattern) ',(sly-current-package))
     nil)))

(defun sly-simple-completions (prefix)
  "Returns list whose car are propertized strings prefix-completing PREFIX"
  (cl-loop with first-difference-pos = (length prefix)
           with (completions common) =
           (sly--completion-request-completions prefix 'slynk-completion:simple-completions)
           for completion in completions
           do (put-text-property first-difference-pos
                                 (min (1+ first-difference-pos)
                                      (1- (length completion))) 
                                 'face
                                 'completions-first-difference
                                 completion)
           collect completion into formatted
           finally return (list formatted common)))

(defun sly-flex-completions (pattern)
  "Returns list whose car is propertized strings flex-completing PATTERN"
  (cl-loop with (completions _) =
           (sly--completion-request-completions pattern 'slynk-completion:flex-completions)
           for (completion _score chunks classification) in completions
           do
           (cl-loop for (pos substring) in chunks
                    do (put-text-property pos (+ pos
                                                 (length substring))
                                          'face
                                          'completions-first-difference
                                          completion))
           (put-text-property 0
                              (length completion)
                              'sly--annotation
                              classification
                              completion)
           collect completion into formatted
           finally return (list formatted nil)))

(defun sly--completion-function-wrapper (fn)
  (let (cached-result cached-arg)
    (lambda (string pred command)
      (cl-flet ((all ()
                     (cl-first
                      (if (and cached-arg
                               (string= cached-arg string))
                          cached-result
                        (setq cached-arg string
                              cached-result (funcall fn string))))))
        (pcase command
          (;; identify this as super-special sly-completion
           ;;
           `sly--identify
           t)
          (;; metadata request
           ;;
           `metadata
           (list 'metadata
                 (cons 'display-sort-function #'identity)
                 (cons 'annotation-function
                       (lambda (completion)
                         (get-text-property 0 'sly--annotation completion)))))
          ;; all completions
          ;; 
          (`t
           (all))
          ;; try completion
          ;;
          (`nil
           (let ((all (all)))
             (if (and all
                      (null (cdr all)))
                 (if (string-prefix-p string (car all))
                     t
                   string)
               (and all
                    string))))
          ;; boundaries
          ;;
          (`(boundaries . ,suffix)
           (completion-boundaries string (all) pred suffix))
          ;; any other value
          ;;
          (_
           nil
           ;; (sly-error "Unrecognized completion command %s" command)
           ))))))

(defun sly--completions-complete-symbol-1 (fn)
  (let* ((beg (sly-symbol-start-pos))
         (end (sly-symbol-end-pos)))
    (list beg end
          (sly--completion-function-wrapper fn))))

(defun sly-simple-complete-symbol ()
  "Prefix completion on the symbol at point.
Intended to go into `completion-at-point-functions'"
  (sly--completions-complete-symbol-1 'sly-simple-completions))

(defun sly-flex-complete-symbol ()
  "\"Flex\" completion on the symbol at point.
Intended to go into `completion-at-point-functions'"
  (sly--completions-complete-symbol-1 'sly-flex-completions))

(defun sly-complete-symbol ()
  "Completion on the symbol at point, using `sly-complete-symbol-function'
Intended to go into `completion-at-point-functions'"
  (sly--completions-complete-symbol-1 sly-complete-symbol-function))

(defun sly-complete-filename-maybe ()
  (when (nth 3 (syntax-ppss)) (comint-filename-completion)))


;;; Set `completion-at-point-functions' and a few other tricks
;;;
(defun sly--completion-setup-target-buffer ()
  (cl-loop for (var . value) in `(;; This one can be customized by a SLY user in `sly-mode-hook'
                                  ;;
                                  (completion-at-point-functions . (sly-complete-filename-maybe
                                                                    sly-complete-symbol))
                                  ;; A custom function for `completion-in-region-function'
                                  ;;
                                  (completion-in-region-function . sly--completion-in-region-function))
           do (set (make-local-variable var) value)))

(add-hook 'sly-mode-hook 'sly--completion-setup-target-buffer)


;;; TODO: Most of the stuff emulates `completion--in-region' and its
;;; callees in Emacs's minibuffer.el
;;; 
(defvar sly--completion-transient-data nil)  ; lifted from `completion-in-region--data' 

(defvar sly--completion-transient-completions nil) ; not used

;;; TODO: not tested with other functions in `completion-at-point-functions'
;;; 
(defun sly--completion-in-region-function (beg end function pred)
  (cond
   ((funcall function nil nil 'sly--identify)
    (let* ((pattern (buffer-substring-no-properties beg end))
           (all
            (all-completions pattern function pred))
           (try
            (try-completion pattern function pred)))
      (setq this-command 'completion-at-point) ; even if we started with `minibuffer-complete'!
      (setq sly--completion-transient-data
            `(,(if (markerp beg) beg (copy-marker beg))
              ,(copy-marker end t)
              ,function
              ,pred))
      (sly--completion-transient-mode 1)
      (setq sly--completion-transient-completions all)
      (cond ((eq try t)
             ;; A unique completions
             ;;
             (choose-completion-string (cl-first all)
                                       (current-buffer)
                                       (list (cl-first sly--completion-transient-data)
                                             (cl-second sly--completion-transient-data)))
             (sly-temp-message 0 2 "Sole completion")
             (sly--completion-hide-completions))
            ;; Incomplete
            ((stringp try)
             (sly--completion-pop-up-completions-buffer pattern all)
             (sly-temp-message 0 2 "Not unique"))
            (t
             (sly-temp-message 0 2 "No completions for %s" pattern)))))
   (t
    (funcall (default-value 'completion-in-region-function)
             beg end function pred))))

(defvar sly--completion-in-region-overlay
  (let ((ov (make-overlay 0 0)))
    (overlay-put ov 'face 'highlight)
    (delete-overlay ov)
    ov)
  "Highlights the currently selected completion candidate")

(defvar sly--completion-display-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'sly-choose-completion)
    (define-key map [mouse-2] 'sly-choose-completion)
    (define-key map [remap previous-line] 'sly-prev-completion)
    (define-key map [remap next-line] 'sly-next-completion)
    (define-key map (kbd "RET") 'sly-choose-completion)
    map)
  "Keymap used in the *sly-completions* buffer")

(define-derived-mode sly--completion-display-mode
  fundamental-mode "SLY Completions"
  "Major mode for presenting SLY completion results.")

(defun sly--completion-transient-mode-postch ()
  "Determine whether to pop down the *sly completions* buffer."
  (unless (or unread-command-events ; Don't pop down the completions in the middle of
                                        ; mouse-drag-region/mouse-set-point.
              
              (and sly--completion-transient-data
                   (and
                    ;; check if we're in the same buffer
                    ;; 
                    (eq (marker-buffer (nth 0 sly--completion-transient-data))
                        (current-buffer))
                    ;; check if point is somewhere acceptably related
                    ;; to the region data that originated the completion
                    ;; 
                    (<= (cl-first sly--completion-transient-data)
                        (point))
                    (<= (point)
                        (cl-second sly--completion-transient-data)))))
    (sly--completion-transient-mode -1)))

(defvar sly--completion-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") 'sly-next-completion)
    (define-key map (kbd "C-p") 'sly-prev-completion)
    (define-key map (kbd "RET") 'sly-choose-completion)
    (define-key map "\t" `(menu-item "" sly-choose-completion
                                     :filter (lambda (original)
                                               (when (memq last-command
                                                           '(completion-at-point
                                                             sly-next-completion
                                                             sly-prev-completion))
                                                 original))))
    (define-key map (kbd "C-g") 'sly-quit-completing)
    map)
  "Keymap used in the buffer originating a *sly-completions* buffer")

(defvar sly--completion-transient-mode nil
  "Explicit `defvar' for `sly--completion-transient-mode'")

(defun sly--completion-turn-off-transient-mode ()
  (if (eq major-mode 'sly--completion-display-mode)
      (sly-message "Choosing completions directly in %s" (current-buffer))
    (setq sly--completion-transient-data nil)
    (sly--completion-hide-completions)))

(define-minor-mode sly--completion-transient-mode
  "Minor mode when the \"*sly completions*\" buffer is showing"
  ;; :lighter " SLY transient completing"
  :variable sly--completion-transient-mode
  :global t
  (remove-hook 'post-command-hook #'sly--completion-transient-mode-postch)
  (setq minor-mode-overriding-map-alist
        (delq (assq 'completion-in-region-mode minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist))
  (if (null sly--completion-transient-mode)
      (sly--completion-turn-off-transient-mode)
    (add-hook 'post-command-hook #'sly--completion-transient-mode-postch)
    (push `(sly--completion-transient-mode . ,sly--completion-transient-mode-map)
          minor-mode-overriding-map-alist)))

;; `define-minor-mode' added to `minor-mode-map-alist', but we wanted
;; `minor-mode-overriding-map-alist' instead, so undo changes to
;; `minor-mode-map-alist'
;; 
(setq minor-mode-map-alist
      (delq (assq 'sly--completion-transient-mode minor-mode-map-alist)
            minor-mode-map-alist))

(defun sly--completion-hide-completions ()
  (let* ((buffer (get-buffer (sly-buffer-name :completions)))
         (win (and buffer
                   (get-buffer-window buffer 0))))
    (when win (with-selected-window win (bury-buffer)))))

(defvar sly--completion-reference-buffer nil
  "Like `completion-reference-buffer', which see")

(defun sly--completion-pop-up-completions-buffer (_pattern completions)
  (let ((display-buffer-mark-dedicated 'soft)
        (pop-up-windows nil)
        completions-buffer first-completion-point)
    (with-displayed-buffer-window
     (sly-buffer-name :completions)
     `((display-buffer--maybe-same-window
        display-buffer-reuse-window
        display-buffer--maybe-pop-up-frame-or-window
        ;; Use `display-buffer-below-selected' for inline completions,
        ;; but not in the minibuffer (e.g. in `eval-expression')
        ;; for which `display-buffer-at-bottom' is used.
        ,(if (eq (selected-window) (minibuffer-window))
             'display-buffer-at-bottom
           'display-buffer-below-selected))
       ,(if temp-buffer-resize-mode
            '(window-height . resize-temp-buffer-window)
          '(window-height . shrink-window-if-larger-than-buffer))
       ,(when temp-buffer-resize-mode
          '(preserve-size . (nil . t))))
     nil
     (sly--completion-transient-mode)
     (let ((reference (current-buffer)))
       (with-current-buffer standard-output
         (sly--completion-display-mode)
         (setq sly--completion-reference-buffer reference)
         (sly--completion-fill-completions-buffer completions)
         (setq completions-buffer standard-output
               first-completion-point (point)))))
    (with-current-buffer completions-buffer
      (goto-char first-completion-point))))

(defvar sly--completion-explanation
  (concat "Use \\[sly-next-completion] and \\[sly-prev-completion] no navigate completions."
          " \\[sly-choose-completion] or [mouse-1] selects a completion."
          "\n\nAnnotation flags: (b)oundp (f)boundp (g)eneric-function (c)lass (m)acro (s)pecial-operator\n\n"))

(defun sly--completion-fill-completions-buffer (completions)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (substitute-command-keys
             sly--completion-explanation))
    (cl-loop with first = (point)
             for completion in completions
             for annotation = (or (get-text-property 0 'sly--annotation completion)
                                  "")
             for start = (point)
             do
             (insert (propertize completion 'mouse-face 'highlight
                                 'sly--completion t))
             (insert (make-string (max
                                   0
                                   (- (window-width)
                                      (length completion)
                                      (length annotation)))
                                  ? )
                     annotation)
             (put-text-property start (point) 'sly--completion completion)
             (insert "\n")
             finally (goto-char first) (sly-next-completion 0))))

(defun sly-next-completion (n &optional errorp)
  (interactive "p")
  (with-current-buffer (sly-buffer-name :completions)
    (unless (zerop n)
      (when (overlay-buffer sly--completion-in-region-overlay)
        (goto-char (overlay-start sly--completion-in-region-overlay)))
      (forward-line n))
    (let* ((end (and (get-text-property (point) 'sly--completion)
                     (save-excursion
                       (skip-syntax-forward "^\s")
                       (point))
                     ;; (next-single-char-property-change (point) 'sly--completion)
                     ))
           (beg (and end
                     (previous-single-char-property-change end 'sly--completion))))
      (if (and beg end)
          (progn
            (move-overlay sly--completion-in-region-overlay
                        beg end)
            (let ((win (get-buffer-window (current-buffer) 0)))
              (when win
                (with-selected-window win
                  (goto-char beg)
                  (sly-recenter beg)))))
        (if errorp
            (sly-error "No completion at point"))))))

(defun sly-prev-completion (n)
  (interactive "p")
  (sly-next-completion (- n)))

(defun sly-choose-completion (&optional event)
  (interactive (list last-nonmenu-event))
  ;; In case this is run via the mouse, give temporary modes such as
  ;; isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (with-current-buffer (sly-buffer-name :completions)
    (when event
      (goto-char (posn-point (event-start event)))
      (sly-next-completion 0 t))
    (let ((completion-text
           (buffer-substring-no-properties (overlay-start sly--completion-in-region-overlay)
                                           (overlay-end sly--completion-in-region-overlay))))
      
      (unless (buffer-live-p sly--completion-reference-buffer)
        (sly-error "Destination buffer is dead"))
      (choose-completion-string completion-text
                                sly--completion-reference-buffer
                                (list (cl-first sly--completion-transient-data)
                                      (cl-second sly--completion-transient-data)))
      (sly--completion-transient-mode -1))))

(defun sly-quit-completing ()
  (interactive)
  (when sly--completion-transient-mode
    (sly--completion-transient-mode -1))
  (keyboard-quit))



;;;; Minibuffer reading

(defvar sly-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'completion-at-point)
    map)
  "Minibuffer keymap used for reading CL expressions.")


(defvar sly-minibuffer-history '()
  "History list of expressions read from the minibuffer.")

(defvar sly-minibuffer-symbol-history '()
  "History list of symbols read from the minibuffer.")

(defmacro sly--with-sly-minibuffer (&rest body)
  `(let ((minibuffer-setup-hook
          (cons (lambda ()
                  (set-syntax-table lisp-mode-syntax-table)
                  (sly--completion-setup-target-buffer))
                minibuffer-setup-hook))
         (sly-buffer-package (sly-current-package))
         (sly-buffer-connection (sly-connection)))
     ,@body))

(defun sly-read-from-minibuffer (prompt &optional initial-value history allow-empty keymap)
  "Read a string from the minibuffer, prompting with PROMPT.
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer
before reading input.  The result is a string (\"\" if no input
was given and ALLOW-EMPTY is non-nil)."
  (sly--with-sly-minibuffer
   (cl-loop
    for i from 0
    for read = (read-from-minibuffer
                (concat "[sly] " (when (cl-plusp i)
                                   "[can't be blank] ")
                        prompt)
                (and (zerop i)
                     initial-value)
                (or keymap sly-minibuffer-map)
                nil (or history 'sly-minibuffer-history))
    when (or (> (length read) 0)
             allow-empty)
    return read)))

(defun sly-read-symbol-name (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (let ((sym-at-point (sly-symbol-at-point)))
    (cond ((or current-prefix-arg query (not sym-at-point))
           (sly--with-sly-minibuffer 
            (completing-read prompt
                             (sly--completion-function-wrapper sly-complete-symbol-function)
                             nil
                             nil
                             sym-at-point)))
          (t sym-at-point))))


(provide 'sly-completion)
;;; sly-completion.el ends here

