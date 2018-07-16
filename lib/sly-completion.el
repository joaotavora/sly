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

(declare-function sly-eval "sly" (sexp &optional package
                                       cancel-on-input
                                       cancel-on-input-retval))

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


;;; Backward compatibility shim for emacs < 25.
;;;
(eval-when-compile
  (unless (fboundp 'with-displayed-buffer-window)
    (defmacro with-displayed-buffer-window (buffer-or-name action quit-function &rest body)
      "Show a buffer BUFFER-OR-NAME and evaluate BODY in that buffer.
This construct is like `with-current-buffer-window' but unlike that
displays the buffer specified by BUFFER-OR-NAME before running BODY."
      (declare (debug t))
      (let ((buffer (make-symbol "buffer"))
            (window (make-symbol "window"))
            (value (make-symbol "value")))
        (macroexp-let2 nil vbuffer-or-name buffer-or-name
          (macroexp-let2 nil vaction action
            (macroexp-let2 nil vquit-function quit-function
              `(let* ((,buffer (temp-buffer-window-setup ,vbuffer-or-name))
                      (standard-output ,buffer)
                      ,window ,value)
                 (with-current-buffer ,buffer
                   (setq ,window (temp-buffer-window-show
                                  ,buffer
                                  ;; Remove window-height when it's handled below.
                                  (if (functionp (cdr (assq 'window-height (cdr ,vaction))))
                                      (assq-delete-all 'window-height (copy-sequence ,vaction))
                                    ,vaction))))

                 (let ((inhibit-read-only t)
                       (inhibit-modification-hooks t))
                   (setq ,value (progn ,@body)))

                 (set-window-point ,window (point-min))

                 (when (functionp (cdr (assq 'window-height (cdr ,vaction))))
                   (ignore-errors
                     (funcall (cdr (assq 'window-height (cdr ,vaction))) ,window)))

                 (if (functionp ,vquit-function)
                     (funcall ,vquit-function ,window ,value)
                   ,value)))))))))



;;; Customization
;;;
(defcustom sly-complete-symbol-function 'sly-flex-completions
  "Function reponsible for SLY completion.
When called with one argument, a pattern, returns a (possibly
propertized) list of strings the complete that pattern,
collected from the Slynk server."
  :type 'function
  :group 'sly-ui)


(cl-defmacro sly--responsive-eval ((var sexp
                                        &optional
                                        package
                                        input-arrived-retval) &rest body)
  "Use `sly-eval' on SEXP, PACKAGE, bind to VAR, run BODY.
If user input arrives in the meantime return INPUT-ARRIVED-RETVAL
immediately."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((sym (make-symbol "sly--responsive-eval")))
    `(let* ((,sym (make-symbol "sly--responsive-eval-unique"))
            (,var (sly-eval ,sexp ,package t ,sym)))
       (if (eq ,var ,sym)
           ,input-arrived-retval
         ,@body))))


;;; Completion calculation
;;;
(defun sly--completion-request-completions (pattern slyfun)
  "Request completions for PATTERN using SLYFUN.
SLYFUN takes two arguments, a pattern and a package."
  (let* ((sly-current-thread t))
    (sly--responsive-eval
        (completions `(,slyfun ,(substring-no-properties pattern)
                               ',(sly-current-package)))
      (or completions
          (progn
	    (when (not (current-message))
	      (sly-message "No completions for %S" pattern))
	    nil)))))

(defun sly-simple-completions (prefix)
  "Return (COMPLETIONS NIL) where COMPLETIONS complete the PREFIX.
COMPLETIONS is a list of propertized strings."
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
  "Return (COMPLETIONS NIL) where COMPLETIONS flex-complete PATTERN.
COMPLETIONS is a list of propertized strings."
  (cl-loop with (completions _) =
           (sly--completion-request-completions pattern 'slynk-completion:flex-completions)
           for (completion score chunks classification) in completions
           do
           (cl-loop for (pos substring) in chunks
                    do (put-text-property pos (+ pos
                                                 (length substring))
                                          'face
                                          'completions-first-difference
                                          completion)
                    collect `(,pos . ,(+ pos (length substring))) into chunks-2
                    finally (put-text-property 0 (length completion)
                                               'sly-completion-chunks chunks-2
                                               completion))
           (put-text-property 0
                              (length completion)
                              'sly--annotation
                              (format "%s %5.2f%%"
                                      classification
                                      (* score 100))
                              completion)

           collect completion into formatted
           finally return (list formatted nil)))

(defun sly-completion-annotation (completion)
  "Grab the annotation of COMPLETION, a string, if any"
  (get-text-property 0 'sly--annotation completion))

;;; backward-compatibility
(defun sly-fuzzy-completions (pattern)
  "This function is obsolete since 1.0.0-beta-2;
use ‘sly-flex-completions’ instead, but notice the updated protocol.

Returns (COMPLETIONS NIL) where COMPLETIONS flex-complete PATTERN.

COMPLETIONS is a list of elements of the form (STRING NIL NIL
ANNOTATION) describing each completion possibility."
  (let ((new (sly-flex-completions pattern)))
    (list (mapcar (lambda (string)
		    (list string nil nil (sly-completion-annotation string)))
		  (car new))
	  (cadr new))))

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
        (cl-case command
          (;; identify this as super-special sly-completion
           ;;
           sly--identify
           t)
          (;; metadata request
           ;;
           metadata (list 'metadata
                          (cons 'display-sort-function #'identity)))
          ;; all completions
          ;;
          ((t)
           (all))
          ;; try completion
          ;;
          ((nil)
           (let ((all (all)))
             (if (and all
                      (null (cdr all)))
                 (if (string-prefix-p string (car all))
                     t
                   string)
               (and all
                    string))))
          ;; boundaries or any other value
          ;;
          (t
           (if (and (consp command)
                    (eq (car command) 'boundaries))
               (completion-boundaries string (all) pred (cdr command))
             ;; (sly-error "Unrecognized completion command %s" command)
             nil)))))))

;; This duplicates a function in sly-parse.el
(defun sly--completion-inside-string-or-comment-p ()
  (let ((ppss (syntax-ppss))) (or (nth 3 ppss) (nth 4 ppss))))

(defun sly--completions-complete-symbol-1 (fn)
  (let* ((beg (sly-symbol-start-pos))
         (end (sly-symbol-end-pos)))
    (list beg end
          (sly--completion-function-wrapper fn)
          :annotation-function #'sly-completion-annotation
          :company-docsig
          (lambda (obj)
            (sly--responsive-eval (arglist `(slynk:operator-arglist
                                             ,(substring-no-properties obj)
                                             ,(sly-current-package)))
              (or (and arglist
                       (sly-autodoc--fontify arglist))
                  "no autodoc information")))
          :company-no-cache t
          :company-doc-buffer
          (lambda (obj)
            (sly--responsive-eval (doc `(slynk:describe-symbol
                                         ,(substring-no-properties obj)))
              (when doc
                (with-current-buffer (get-buffer-create " *sly-completion doc*")
                  (erase-buffer)
                  (insert doc)
                  (current-buffer)))))
          :company-require-match 'never
          :company-match
          (lambda (obj)
            (get-text-property 0 'sly-completion-chunks obj))
          :company-location
          (lambda (obj)
            (save-window-excursion
              (let* ((buffer (sly-edit-definition
                              (substring-no-properties obj))))
                (when (buffer-live-p buffer) ; on the safe side
                  (cons buffer (with-current-buffer buffer
                                 (point)))))))
          :company-prefix-length
          (and (sly--completion-inside-string-or-comment-p) 0))))

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
                                  (completion-in-region-function . sly--completion-in-region-function)
                                  ;; Support emacs 24.3
                                  ;;
                                  ,@(when (version< emacs-version "24.4")
                                      `((completion-list-insert-choice-function
					 .
					 (lambda (beg end newtext)
					   (goto-char beg)
					   (delete-region beg end)
					   (insert newtext)))
					(completion-in-region-functions
                                         .
                                         (,(lambda (_next &rest args)
                                             (apply #'sly--completion-in-region-function args)))))))
           do (set (make-local-variable var) value)))

(add-hook 'sly-mode-hook 'sly--completion-setup-target-buffer)


;;; TODO: Most of the stuff emulates `completion--in-region' and its
;;; callees in Emacs's minibuffer.el
;;; 
(defvar sly--completion-transient-data nil)  ; similar to `completion-in-region--data'

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
      (setq sly--completion-transient-completions all)
      (cond ((eq try t)
             ;; A unique completion
             ;;
             (choose-completion-string (cl-first all)
                                       (current-buffer)
                                       (list beg end))
             (sly-temp-message 0 2 "Sole completion"))
            ;; Incomplete
            ((stringp try)
             (let ((pattern-overlay (make-overlay beg end nil nil nil)))
               (setq sly--completion-transient-data
                     `(,pattern-overlay
                       ,function
                       ,pred))
               (overlay-put pattern-overlay 'face 'highlight)
               (sly--completion-pop-up-completions-buffer pattern all)
               (sly-temp-message 0 2 "Not unique")
               (sly--completion-transient-mode 1)))
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
    (define-key map [backtab]     'sly-prev-completion)
    (define-key map (kbd "q") 'sly-completion-hide-completions)
    (define-key map (kbd "C-g") 'sly-completion-hide-completions)
    (define-key map (kbd "z") 'sly-completion-hide-completions)
    (define-key map [remap previous-line] 'sly-prev-completion)
    (define-key map [remap next-line] 'sly-next-completion)
    (define-key map [left] 'sly-prev-completion)
    (define-key map [right] 'sly-next-completion)
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
              (let ((pattern-ov
                     (and sly--completion-transient-data
                          (car
                           sly--completion-transient-data))))
                (and pattern-ov
                     ;; check if we're in the same buffer
                     ;;
                     (eq (overlay-buffer pattern-ov)
                         (current-buffer))
                     ;; check if point is somewhere acceptably related
                     ;; to the region data that originated the completion
                     ;;
                     (<= (overlay-start pattern-ov)
                         (point)
                         (overlay-end pattern-ov)))))
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
    (sly-completion-hide-completions)))

(define-minor-mode sly--completion-transient-mode
  "Minor mode when the \"*sly completions*\" buffer is showing"
  ;; :lighter " SLY transient completing"
  :variable sly--completion-transient-mode
  :global t
  (remove-hook 'post-command-hook #'sly--completion-transient-mode-postch)
  (setq display-buffer-alist
        (delq (assq 'sly--completion-transient-mode-display-guard-p display-buffer-alist)
              display-buffer-alist))
  (setq minor-mode-overriding-map-alist
        (delq (assq 'completion-in-region-mode minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist))
  (if (null sly--completion-transient-mode)
      (sly--completion-turn-off-transient-mode)
    (add-hook 'post-command-hook #'sly--completion-transient-mode-postch)
    (push `(sly--completion-transient-mode . ,sly--completion-transient-mode-map)
          minor-mode-overriding-map-alist)
    (push `(sly--completion-transient-mode-display-guard-p
            (sly--completion-transient-mode-teardown-before-displaying
             . ,display-buffer-alist))
          display-buffer-alist)))

;; `define-minor-mode' added to `minor-mode-map-alist', but we wanted
;; `minor-mode-overriding-map-alist' instead, so undo changes to
;; `minor-mode-map-alist'
;;
(setq minor-mode-map-alist
      (delq (assq 'sly--completion-transient-mode minor-mode-map-alist)
            minor-mode-map-alist))

;; displaying other buffers with pop-to-buffer while in
;; `sly--completion-transient-mode' is problematic, because it
;; dedicates a window. Try some crazy `display-buffer-alist' shit to
;; prevent that.
;;
(defun sly--completion-transient-mode-display-guard-p (buffer-name _action)
  (not (string-match-p "^*sly-completions*" buffer-name)))

(defun sly--completion-transient-mode-teardown-before-displaying (_buffer _alist)
  (sly--completion-transient-mode -1)
  ;; returns nil, hoping some other function in alist will display the
  ;; buffer as intended.
  nil)

(defun sly--completion-kill-transient-data ()
  (when (overlayp (car sly--completion-transient-data))
    (delete-overlay (car sly--completion-transient-data)))
  (setq sly--completion-transient-data nil))

(defun sly-completion-hide-completions ()
  (interactive)
  (sly--completion-kill-transient-data)
  (let* ((buffer (get-buffer (sly-buffer-name :completions)))
         (win (and buffer
                   (get-buffer-window buffer 0))))
    (when win (with-selected-window win (quit-window t)))))

(defvar sly--completion-reference-buffer nil
  "Like `completion-reference-buffer', which see")

(defmacro sly--completion-with-displayed-buffer-window (buffer
                                                        action
                                                        quit-function
                                                        &rest body)
  ;;; WITH-DISPLAYED-BUFFER-WINDOW doesn't work noninteractively
  (let ((original-sym (cl-gensym "original-buffer-")))
    `(if noninteractive
         (let ((,original-sym (current-buffer)))
           (display-buffer (get-buffer-create ,buffer) ,action)
           (let ((standard-output ,buffer))
             (with-current-buffer ,original-sym
               ,@body)))
       (with-displayed-buffer-window ,buffer ,action ,quit-function
                                     ,@body))))

(defun sly--completion-pop-up-completions-buffer (_pattern completions)
  (let ((display-buffer-mark-dedicated 'soft)
        (pop-up-windows nil)
        completions-buffer first-completion-point)
    (sly--completion-with-displayed-buffer-window
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
         (set (make-local-variable 'cursor-type) nil)
         (setq sly--completion-reference-buffer reference)
         (sly--completion-fill-completions-buffer completions)
         (setq completions-buffer standard-output
               first-completion-point (point))
         (add-hook 'kill-buffer-hook 'sly--completion-kill-transient-data t t))))
    (with-current-buffer completions-buffer
      (goto-char first-completion-point))))

(defvar sly--completion-explanation
  (concat "Use \\[sly-next-completion] and \\[sly-prev-completion] to navigate completions."
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
                                   1
                                   (- (1- (window-width))
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
                                (let ((pattern-ov
                                       (car sly--completion-transient-data)))
                                  (list (overlay-start pattern-ov)
                                        (overlay-end pattern-ov))))
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

