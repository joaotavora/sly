;; -*- lexical-binding: t -*- An experimental implementation of
;; multiple REPLs multiplexed over a single Slime socket.  M-x
;; sly-mrepl or M-x sly-mrepl-new create new REPL buffers.
;;
(require 'sly)
(require 'cl-lib)
(require 'comint)
(define-sly-contrib sly-mrepl
  "Multiple REPLs."
  (:license "GPL")
  (:slynk-dependencies slynk-mrepl)
  (:on-load
   ;; Define a new "part action" for the `sly-part' buttons and change
   ;; the `sly-inspector-part', `sly-db-local-variable' and
   ;; `sly-trace-dialog-part' to include it.
   ;;
   (sly-button-define-part-action sly-mrepl-copy-part-to-repl
                                  "Copy to REPL" (kbd "M-RET"))
   (sly-button-define-part-action sly-mrepl-copy-call-to-repl
                                  "Copy call to REPL" (kbd "M-S-<return>"))
   (button-type-put 'sly-inspector-part
                    'sly-mrepl-copy-part-to-repl
                    'sly-inspector-copy-part-to-repl)
   (button-type-put 'sly-db-local-variable
                    'sly-mrepl-copy-part-to-repl
                    'sly-db-copy-part-to-repl)
   (button-type-put 'sly-apropos-symbol
                    'sly-mrepl-copy-part-to-repl
                    'sly-apropos-copy-symbol-to-repl)
   (button-type-put 'sly-db-frame
                    'sly-mrepl-copy-call-to-repl
                    'sly-db-copy-call-to-repl)
   (eval-after-load "sly-trace-dialog"
     `(progn
        (button-type-put 'sly-trace-dialog-part
                         'sly-mrepl-copy-part-to-repl
                         'sly-trace-dialog-copy-part-to-repl)
        (button-type-put 'sly-trace-dialog-spec
                         'sly-mrepl-copy-call-to-repl
                         'sly-trace-dialog-copy-call-to-repl)))
   ;; Make C-c ~ bring popup REPL
   ;;
   (define-key sly-mode-map (kbd "C-c ~") 'sly-mrepl-sync)
   (define-key sly-mode-map (kbd "C-c C-z") 'sly-mrepl)
   (define-key sly-selector-map (kbd "~")  'sly-mrepl-sync)
   (define-key sly-selector-map (kbd "r") 'sly-mrepl)
   
   ;; Insinuate ourselves in hooks
   ;;
   (add-hook 'sly-connected-hook 'sly-mrepl-on-connection)
   (add-hook 'sly-net-process-close-hooks 'sly-mrepl--teardown-repls)
   ;; The connection list is also tweaked
   ;;
   (setq sly-connection-list-button-action
         #'(lambda (process)
             (let ((sly-default-connection process))
               (sly-mrepl 'interactive)))))
  (:on-unload
   ;; FIXME: This `:on-unload' is grossly incomplete
   ;; 
   (remove-hook 'sly-connected-hook 'sly-mrepl-on-connection)
   (remove-hook 'sly-net-process-close-hooks 'sly-mrepl--teardown-repls)))


;; User-visible variables
;;
(defvar sly-mrepl-mode-hook nil
  "Functions run after `sly-mrepl-mode' is set up")

(defvar sly-mrepl-hook nil
  "Functions run after `sly-mrepl-new' sets up a REPL.")

(defvar sly-mrepl-runonce-hook nil
  "Functions run once after `sly-mrepl-new' sets up a REPL.

After running the contents of this hook its default value is
emptied. See also `sly-mrepl-hook'")

(defvar sly-mrepl-output-filter-functions comint-preoutput-filter-functions
  "List of functions filtering Slynk's REPL output.
This variables behaves like `comint-preoutput-filter-functions',
for output printed to the REPL (not for evaluation results)")

(defcustom sly-mrepl-history-file-name (expand-file-name "~/.sly-mrepl-history")
  "The file used to store MREPL's input history across sessions."
  :group 'sly)

(defvar sly-mrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     'sly-mrepl-return)
    (define-key map [return]        'sly-mrepl-return)
    (define-key map (kbd "TAB")     'sly-mrepl-indent-and-complete-symbol)
    (define-key map (kbd "C-c C-b") 'sly-interrupt)
    (define-key map (kbd "C-c C-c") 'sly-interrupt)
    (define-key map (kbd "M-p")     'sly-mrepl-previous-input-or-button)
    (define-key map (kbd "M-n")     'sly-mrepl-next-input-or-button)
    (define-key map (kbd "C-M-p")     'sly-button-backward)
    (define-key map (kbd "C-M-n")     'sly-button-forward)
    map))

(defvar sly-mrepl-pop-sylvester 'on-connection)

(defface sly-mrepl-prompt-face
  `((t (:inherit font-lock-builtin-face)))
  "Face for the regular MREPL prompt."
  :group 'sly-mode-faces)

(defface sly-mrepl-note-face
  `((t (:inherit font-lock-keyword-face)))
  "Face for the MREPL notes."
  :group 'sly-mode-faces)

(defface sly-mrepl-output-face
  '((((class color)
      (background dark))
     (:foreground "VioletRed1"))
    (((class color)
      (background light))
     (:foreground "steel blue"))
    (t
     (:bold t :italic t)))
  "Face for the regular MREPL prompt."
  :group 'sly-mode-faces)


;; Internal variables
;;
(defvar sly-mrepl--remote-channel nil)
(defvar sly-mrepl--local-channel nil)
(defvar sly-mrepl--read-mode nil)
(defvar sly-mrepl--output-mark nil)
(defvar sly-mrepl--dedicated-stream nil)
(defvar sly-mrepl--last-prompt-overlay nil)
(defvar sly-mrepl--pending-output nil
  "Output that can't be inserted right now.")
(defvar sly-mrepl--dedicated-stream-hooks)
(defvar sly-mrepl--history-separator "####\n")
(defvar sly-mrepl--dirty-history nil)


;; Major mode
;;
(define-derived-mode sly-mrepl-mode comint-mode "mrepl"
  (sly-mode 1)
  (cl-loop for (var value)
           in `((comint-use-prompt-regexp nil)
                (comint-inhibit-carriage-motion t)
                (comint-input-sender sly-mrepl--input-sender)
                (comint-output-filter-functions nil)
                (comint-input-filter-functions nil)
                (comint-history-isearch dwim)
                (comint-input-ring-file-name ,sly-mrepl-history-file-name)
                (comint-input-ignoredups t)
                (comint-prompt-read-only t)
                (indent-line-function lisp-indent-line)
                (sly-mrepl--read-mode nil)
                (sly-mrepl--pending-output nil)
                (sly-mrepl--output-mark ,(point-marker))
                (sly-mrepl--last-prompt-overlay ,(make-overlay 0 0 nil nil))
                (sly-find-buffer-package-function sly-mrepl-guess-package)
                (sly-autodoc-inhibit-autodoc sly-mrepl-inside-string-or-comment-p)
                (mode-line-process nil)
                (parse-sexp-ignore-comments t)
                (syntax-propertize-function sly-mrepl--syntax-propertize)
                (forward-sexp-function sly-mrepl--forward-sexp)
                (comint-scroll-show-maximum-output nil)
                (comint-scroll-to-bottom-on-input nil)
                (comint-scroll-to-bottom-on-output nil)
                (inhibit-field-text-motion nil)
                (lisp-indent-function common-lisp-indent-function)
                (open-paren-in-column-0-is-defun-start nil)
                (buffer-file-coding-system utf-8-unix))
           do (set (make-local-variable var) value))
  (set-marker-insertion-type sly-mrepl--output-mark nil)
  (add-hook 'kill-emacs-hook 'sly-mrepl--save-all-histories)
  ;;(set (make-local-variable 'comint-get-old-input) 'ielm-get-old-input)
  (set-syntax-table lisp-mode-syntax-table)
  (set-keymap-parent sly-mrepl-mode-map nil)

  ;; Add hooks to isearch-mode placed strategically after the ones
  ;; set by comint.el itself.
  ;;
  (add-hook 'isearch-mode-hook 'sly-mrepl--setup-comint-isearch t t)
  (add-hook 'isearch-mode-end-hook 'sly-mrepl--teardown-comint-isearch t t)

  ;; Add a post-command-handler
  ;;
  (add-hook 'post-command-hook 'sly-mrepl--highlight-backreferences-maybe t t))


;;; Channel methods
(sly-define-channel-type listener)

(sly-define-channel-method listener :write-values (results)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--insert-results results)))

(sly-define-channel-method listener :evaluation-aborted (&optional condition)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--catch-up)
    (sly-mrepl--insert-note (format "Evaluation aborted on %s" condition))))

(sly-define-channel-method listener :write-string (string)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--insert-output string)))

(sly-define-channel-method listener :set-read-mode (mode)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--accept-process-output)
    (let ((inhibit-read-only t))
      (cl-ecase mode
        (:read (setq sly-mrepl--read-mode (point))
               (add-text-properties (1- (point)) (point)
                                    `(rear-nonsticky t))
               (sly-message "Listener waiting for input to read"))
        (:eval (if sly-mrepl--read-mode
                   (add-text-properties (1- sly-mrepl--read-mode) (point)
                                        `(face bold read-only t))
                 (sly-warning "Expected `sly-mrepl--read-mode' to be set!"))
               (sly-mrepl--catch-up)
               (setq sly-mrepl--read-mode nil)
               (when sly-mrepl--pending-output
                 (sly-mrepl--insert-output "\n"))
               (sly-message "Listener waiting for sexps to eval"))))))

(sly-define-channel-method listener :prompt (package prompt
                                                     error-level
                                                     &optional condition)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--insert-prompt package prompt error-level condition)))

(sly-define-channel-method listener :open-dedicated-output-stream (port _coding-system)
  (with-current-buffer (sly-channel-get self 'buffer)
    ;; HACK: no coding system
    (set (make-local-variable 'sly-mrepl--dedicated-stream)
         (sly-mrepl--open-dedicated-stream self port nil))))

(sly-define-channel-method listener :clear-repl-history ()
  (with-current-buffer (sly-channel-get self 'buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (sly-mrepl--insert-note "Cleared REPL history"))))


;;; Button type
;;;
(define-button-type 'sly-mrepl-part :supertype 'sly-part
  'sly-button-inspect
  #'(lambda (entry-idx value-idx)
      (sly-eval-for-inspector `(slynk-mrepl:inspect-entry
                                ,sly-mrepl--remote-channel
                                ,entry-idx
                                ,value-idx)))
  'sly-button-describe
  #'(lambda (entry-idx value-idx)
      (sly-eval-describe `(slynk-mrepl:describe-entry ,sly-mrepl--remote-channel
                                                      ,entry-idx
                                                      ,value-idx)))
  'sly-button-pretty-print
  #'(lambda (entry-idx value-idx)
      (sly-eval-describe `(slynk-mrepl:pprint-entry ,sly-mrepl--remote-channel
                                                      ,entry-idx
                                                      ,value-idx)))
  'sly-mrepl-copy-part-to-repl 'sly-mrepl--copy-part-to-repl)


;;; Internal functions
;;;
(defun sly-mrepl--buffer-name (connection &optional handle)
  (sly-buffer-name :mrepl :connection connection
                   :suffix handle))

(defun sly-mrepl--teardown-repls (process)
  (cl-loop for buffer in (buffer-list)
           when (buffer-live-p buffer)
           do (with-current-buffer buffer
                (when (and (eq major-mode 'sly-mrepl-mode)
                           (eq sly-buffer-connection process))
                  (sly-mrepl--teardown (process-get process 'sly-net-close-reason))))))

(defun sly-mrepl--process () (get-buffer-process (current-buffer))) ;stupid

(defun sly-mrepl--mark ()
  "Returns a marker to the end of the last prompt."
  (process-mark (sly-mrepl--process)))

(defun sly-mrepl--safe-mark ()
  "Like `sly-mrepl--mark', but safe if there's no process."
  (if (sly-mrepl--process) (sly-mrepl--mark) (point-max)))

(defmacro sly-mrepl--commiting-text (props &rest body)
  (declare (debug (sexp &rest form))
           (indent 1))
  (let ((start-sym (cl-gensym)))
    `(let ((,start-sym (marker-position (sly-mrepl--mark)))
           (inhibit-read-only t))
       ,@body
       (add-text-properties ,start-sym (sly-mrepl--mark)
                            (append '(read-only t front-sticky (read-only))
                                    ,props)))))

(defun sly-mrepl--forward-sexp (n)
  "Just like `forward-sexp' unless point it at prompt start.
In that case, moving a sexp backward does nothing."
  (if (or (cl-plusp n)
          (/= (point) (sly-mrepl--safe-mark)))
      (let ((forward-sexp-function nil))
        (forward-sexp n))))

(defun sly-mrepl--syntax-propertize (beg end)
  "Make everything up to current prompt comment syntax."
  (remove-text-properties beg end '(syntax-table nil))
  (let ((end (min end (sly-mrepl--safe-mark)))
        (beg beg))
    (when (> end beg)
      (unless (nth 8 (syntax-ppss beg))
        (add-text-properties beg (1+ beg)
                             `(syntax-table ,(string-to-syntax "!"))))
      (add-text-properties (1- end) end
                           `(syntax-table ,(string-to-syntax "!"))))))

(defun sly-mrepl--call-with-repl (repl-buffer fn)
  (with-current-buffer repl-buffer
    (cl-loop
     while (not (buffer-local-value 'sly-mrepl--remote-channel
                                    (current-buffer)))
     do
     (sly-warning "Waiting for a REPL to be setup for %s"
                  (sly-connection-name (sly-current-connection)))
     (sit-for 0.5))
    (funcall fn)))

(defmacro sly-mrepl--with-repl (repl-buffer &rest body)
  (declare (indent 1) (debug (sexp &rest form)))
  `(sly-mrepl--call-with-repl ,repl-buffer #'(lambda () ,@body)))

(defun sly-mrepl--insert (string &optional face)
  (sly-mrepl--commiting-text (when face
                               `(face ,face font-lock-face ,face))
    (comint-output-filter (sly-mrepl--process)
                          (propertize string 'sly-mrepl-break-output t))))

(defun sly-mrepl--break-output-p (pos)
  (and (not (eq ?\n (char-after pos)))
       (get-char-property pos 'sly-mrepl-break-output)))

(defun sly-mrepl--insert-output (string &optional face nofilters)
  (cond ((and (not sly-mrepl--read-mode) string)
         (let ((inhibit-read-only t)
               (start (marker-position sly-mrepl--output-mark))
               (face (or face
                         'sly-mrepl-output-face)))
           
           (save-excursion
             (goto-char sly-mrepl--output-mark)
             (cond ((and (not (bobp))
                         (sly-mrepl--break-output-p (1- start))
                         (not (zerop (current-column))))
                    (insert-before-markers "\n")))
             (setq string
                   (propertize (concat sly-mrepl--pending-output string)
                               'face face
                               'font-lock-face face))
             (setq sly-mrepl--pending-output nil)
             (unless nofilters
               (mapc (lambda (fn)
                       (setq string (funcall fn string)))
                     sly-mrepl-output-filter-functions))
             (insert-before-markers string)
             (cond ((and (not (zerop (current-column)))
                         (sly-mrepl--break-output-p (point)))
                    (save-excursion (insert "\n"))))
             (add-text-properties start sly-mrepl--output-mark
                                  `(read-only t front-sticky (read-only)
                                              field sly-mrepl--output)))))
        (t
         (setq sly-mrepl--pending-output
               (concat sly-mrepl--pending-output string))
         (sly-message "Some output saved for later insertion"))))

(defun sly-mrepl--insert-note (string &optional face)
  (let* ((face (or face 'sly-mrepl-note-face))
         (string (replace-regexp-in-string "^" "; " string)))
    (cond ((sly-mrepl--process)
           ;; notes are inserted "synchronously" with the process mark  process
           (sly-mrepl--ensure-newline)
           (sly-mrepl--insert string face))
          (t
           ;; If no process yet, fall back to the simpler strategy.
           (sly-mrepl--insert-output string face)))))

(defun sly-mrepl--send-input-sexp ()
  (goto-char (point-max))
  (save-excursion
    (skip-chars-backward "\n\t\s")
    (delete-region (max (point)
                        (sly-mrepl--mark))
                   (point-max)))
  (buffer-disable-undo)
  (overlay-put sly-mrepl--last-prompt-overlay 'face 'highlight)
  (set (make-local-variable 'sly-mrepl--dirty-history) t)
  (sly-mrepl--commiting-text
      `(field sly-mrepl-input
              keymap ,(let ((map (make-sparse-keymap)))
                        (define-key map (kbd "RET") 'sly-mrepl-insert-input)
                        (define-key map [return] 'sly-mrepl-insert-input)
                        (define-key map [mouse-2] 'sly-mrepl-insert-input)
                        map))
    (comint-send-input))
  (sly-mrepl--ensure-prompt-face))

(defun sly-mrepl--ensure-newline ()
  (unless (save-excursion
            (goto-char (sly-mrepl--mark))
            (zerop (current-column)))
    (sly-mrepl--insert "\n")))

(defun sly-mrepl--accept-process-output ()
  (when (and sly-mrepl--dedicated-stream
             (process-live-p sly-mrepl--dedicated-stream))
    ;; This non-blocking call should be enough to allow asynch calls
    ;; to `sly-mrepl--insert-output' to still see the correct value
    ;; for `sly-mrepl--output-mark' just before we call
    ;; `sly-mrepl--catch-up'.
    (while (accept-process-output sly-mrepl--dedicated-stream
                                  0
                                  (and (eq (window-system) 'w32) 1)))))

(defun sly-mrepl--ensure-prompt-face ()
  "Override `comint.el''s use of `comint-highlight-prompt'."
  (let ((inhibit-read-only t))
    (add-text-properties (overlay-start sly-mrepl--last-prompt-overlay)
                         (overlay-end sly-mrepl--last-prompt-overlay)
                         '(font-lock-face sly-mrepl-prompt-face))))

(defun sly-mrepl--insert-prompt (package prompt error-level &optional condition)
  (sly-mrepl--accept-process-output)
  (overlay-put sly-mrepl--last-prompt-overlay 'face 'bold)
  (when condition
    (sly-mrepl--insert-note (format "Debugger entered on %s" condition)))
  (sly-mrepl--ensure-newline)
  (sly-mrepl--catch-up)
  (let ((beg (marker-position (sly-mrepl--mark))))
    (sly-mrepl--insert
     (propertize
      (concat
       (when (cl-plusp error-level)
         (concat (sly-make-action-button
                  (format "[%d]" error-level)
                  #'sly-db-pop-to-debugger-maybe)
                 " "))
       (propertize
        (concat prompt "> ")
        'face 'sly-mrepl-prompt-face
        'font-lock-face 'sly-mrepl-prompt-face))
      'sly-mrepl--prompt (downcase package)))
    (move-overlay sly-mrepl--last-prompt-overlay beg (sly-mrepl--mark)))
  (sly-mrepl--ensure-prompt-face)
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun sly-mrepl--copy-part-to-repl (entry-idx value-idx)
  (sly-mrepl--copy-objects-to-repl `(,entry-idx ,value-idx)
                                   (format "Returning value %s of history entry %s"
                                           value-idx entry-idx)))

(cl-defun sly-mrepl--eval-for-repl (slyfun-and-args &key insert-p before-prompt after-prompt)
  "Evaluate SLYFUN-AND-ARGS in Slynk, then call callbacks.

SLYFUN-AND-ARGS is (SLYFUN . ARGS) and is called in
Slynk. SLYFUN's multiple return values are captured in a list and
passed to the optional unary callbacks BEFORE-PROMPT and
AFTER-PROMPT, called before or after prompt insertion,
respectively.

If INSERT-P is non-nil, SLYFUN's results are printable
representations of Slynk objects and should be inserted into the
REPL."
  (sly-eval-async `(slynk-mrepl:eval-for-mrepl
                    ,sly-mrepl--remote-channel
                    ',(car slyfun-and-args)
                    ,@(cdr slyfun-and-args))
    (lambda (prompt-args-and-results)
      (cl-destructuring-bind (prompt-args results)
          prompt-args-and-results
        (goto-char (sly-mrepl--mark))
        (let ((saved-text (buffer-substring (point) (point-max))))
          (delete-region (point) (point-max))
          (sly-mrepl--catch-up)
          (when before-prompt
            (funcall before-prompt results))
          (when insert-p
            (sly-mrepl--insert-results results))
          (apply #'sly-mrepl--insert-prompt prompt-args)
          (when after-prompt
            (funcall after-prompt results))
          (pop-to-buffer (current-buffer))
          (goto-char (sly-mrepl--mark))
          (insert saved-text))))))

(defun sly-mrepl--copy-objects-to-repl (method-args &optional before after)
  "Recall objects in the REPL history as a new entry.
METHOD-ARGS are SWANK-MREPL:COPY-TO-REPL's optional args. If nil
then the globally saved objects that
SLYNK-MREPL:GLOBALLY-SAVE-OBJECT stored are considered, otherwise
it is a list (ENTRY-IDX VALUE-IDX).
BEFORE and AFTER as in `sly-mrepl--save-and-copy-for-repl'"
  (sly-mrepl--eval-for-repl
   `(slynk-mrepl:copy-to-repl
     ,@method-args)
   :before-prompt (if (stringp before)
                      (lambda (objects)
                        (sly-mrepl--insert-note before)
                        (sly-mrepl--insert-results objects))
                    before)
   :after-prompt after))

(defun sly-mrepl--make-result-button (result idx)
  (make-text-button (car result) nil
                    :type 'sly-mrepl-part
                    'part-args (list (cadr result) idx)
                    'part-label (format "REPL Result")
                    'sly-mrepl--result result
                    'sly-button-search-id (sly-button-next-search-id))
  (car result))

(defun sly-mrepl--insert-results (results)
  (let* ((comint-preoutput-filter-functions nil))
    (if (null results)
        (sly-mrepl--insert-note "No values")
      (cl-loop for result in results
               for idx from 0
               do
               (sly-mrepl--ensure-newline)
               (sly-mrepl--insert (sly-mrepl--make-result-button result idx))))))

(defun sly-mrepl--catch-up ()
  (when (> (sly-mrepl--mark) sly-mrepl--output-mark)
    (set-marker sly-mrepl--output-mark (sly-mrepl--mark))))

(defun sly-mrepl--input-sender (_proc string)
  (sly-mrepl--send-string (substring-no-properties string)))

(defun sly-mrepl--send-string (string &optional _command-string)
  (sly-mrepl--send `(:process ,string)))

(defun sly-mrepl--send (msg)
  "Send MSG to the remote channel."
  (sly-send-to-remote-channel sly-mrepl--remote-channel msg))

(defun sly-mrepl--find-buffer (&optional connection thread)
  "Find the shortest-named (default) `sly-mrepl' buffer for CONNECTION."
  ;; CONNECTION defaults to the `sly-default-connection' passing
  ;; through `sly-connection'. Seems to work OK...
  ;;
  (let* ((connection (or connection
                         (let ((sly-buffer-connection nil)
                               (sly-dispatching-connection nil))
                           (sly-connection))))
         (repls (cl-remove-if-not (lambda (x)
                                    (with-current-buffer x
                                      (and (eq major-mode 'sly-mrepl-mode)
                                           (eq sly-buffer-connection connection)
                                           (or (not thread)
                                               (eq thread sly-current-thread)))))
                                  (buffer-list)))
         (sorted (cl-sort repls #'< :key (sly-compose #'length #'buffer-name))))
    (car sorted)))

(defun sly-mrepl--find-create (connection)
  (or (sly-mrepl--find-buffer connection)
      (sly-mrepl-new connection)))

(defun sly-mrepl--busy-p ()
  (>= sly-mrepl--output-mark (sly-mrepl--mark)))

(defun sly-mrepl--read-input-ring ()
  (let ((comint-input-ring-separator sly-mrepl--history-separator))
    (comint-read-input-ring)))

(defcustom sly-mrepl-prevent-duplicate-history 'move
  "If non-nil, prevent duplicate entries in input history.

Otherwise (if nil), input entry are always added to the end of
the history, even if they already occur in the history.

If the non-nil value is `move', the previously occuring entry is
discarded, i.e. moved to a more recent spot. Any other non-nil
value laves the previous entry untouched and it is the more
recent entry that is discarded."
  :group 'sly)

(defun sly-mrepl--merge-and-save-history ()
  (let*
      ;; To merge the file's history with the current buffer's
      ;; history, start by deep-copying `comint-input-ring' to a
      ;; separate variable.
      ;; 
      ((current-ring (copy-tree comint-input-ring 'vectors-too))
       (index (ring-length current-ring))
       (comint-input-ring-separator sly-mrepl--history-separator))
    ;; this sets `comint-input-ring' from the file
    ;;
    (sly-mrepl--read-input-ring)
    ;; loop `current-ring', which potentially contains new entries and
    ;; re-add entries to `comint-input-ring', which is now synched
    ;; with the file and will be written to disk. Respect
    ;; `sly-mrepl-prevent-duplicate-history'.
    ;; 
    (cl-loop for i from (1- index) downto 0
             for item = (ring-ref current-ring i)
             for existing-index = (ring-member comint-input-ring item)
             do (cond ((and existing-index
                            (eq sly-mrepl-prevent-duplicate-history 'move))
                       (ring-remove comint-input-ring existing-index)
                       (ring-insert comint-input-ring item))
                      ((and existing-index
                            (not sly-mrepl-prevent-duplicate-history))
                       (ring-insert comint-input-ring item))
                      (t
                       (ring-insert comint-input-ring item)))
             unless (ring-member comint-input-ring item)
             do (ring-insert comint-input-ring item))
    ;; Now save `comint-input-ring'
    (let ((coding-system-for-write 'utf-8-unix))
      (comint-write-input-ring))
    (set (make-local-variable 'sly-mrepl--dirty-history) nil)))

(defun sly-mrepl--save-all-histories ()
  (cl-loop for buffer in (buffer-list)
           do
           (with-current-buffer buffer
             (when (and (eq major-mode 'sly-mrepl-mode)
                        sly-mrepl--dirty-history)
               (sly-mrepl--merge-and-save-history)))))

(defun sly-mrepl--teardown (&optional reason)
  (remove-hook 'kill-buffer-hook 'sly-mrepl--teardown t)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (let ((start (point)))
      (unless (zerop (current-column)) (insert "\n"))
      (insert (format "; %s" (or reason "REPL teardown")))
      (unless (zerop (current-column)) (insert "\n"))
      (insert "; --------------------------------------------------------\n")
      (add-text-properties start (point) '(read-only t))))
  (sly-mrepl--merge-and-save-history)
  (when sly-mrepl--dedicated-stream
    (process-put sly-mrepl--dedicated-stream 'sly-mrepl--channel nil)
    (kill-buffer (process-buffer sly-mrepl--dedicated-stream)))
  ;; signal lisp that we're closingq
  (when (ignore-errors (sly-connection))
    (sly-mrepl--send `(:teardown))
    (sly-close-channel sly-mrepl--local-channel))
  (set (make-local-variable 'sly-mrepl--remote-channel) nil)
  (when (sly-mrepl--process)
    (delete-process (sly-mrepl--process))))

(defun sly-mrepl--dedicated-stream-output-filter (process string)
  (let* ((channel (process-get process 'sly-mrepl--channel))
         (buffer (and channel
                      (sly-channel-get channel 'buffer))))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (cl-plusp (length string))
                     (eq (process-status sly-buffer-connection) 'open))
            (sly-mrepl--insert-output string)))
      (sly-warning "No channel in process %s, probaly torn down" process))))

(defun sly-mrepl--open-dedicated-stream (channel port coding-system)
  (let* ((name (format "sly-dds-%s-%s"
                       (process-get sly-buffer-connection
                                    'sly--net-connect-counter)
                       (sly-channel.id channel)))
         (stream (open-network-stream name
                                      (generate-new-buffer
                                       (format " *%s*" name))
                                      (car (process-contact sly-buffer-connection))
                                      port))
         (emacs-coding-system (car (cl-find coding-system
                                            sly-net-valid-coding-systems
                                            :key #'cl-third))))
    (set-process-query-on-exit-flag stream nil)
    (set-process-plist stream `(sly-mrepl--channel ,channel))
    (set-process-filter stream 'sly-mrepl--dedicated-stream-output-filter)
    (set-process-coding-system stream emacs-coding-system emacs-coding-system)
    (sly--when-let (secret (sly-secret))
      (sly-net-send secret stream))
    (run-hook-with-args 'sly-mrepl--dedicated-stream-hooks stream)
    stream))

(cl-defun sly-mrepl--save-and-copy-for-repl (slyfun-and-args &key repl before after)
  "Evaluate SLYFUN-AND-ARGS in Slynk and prepare to copy to REPL.
BEFORE is a string inserted as a note, or a nullary function
which is run just before the object is copied to the
REPL. Optional BEFORE and AFTER are unary functions called with a
list of the saved values' presentations strings and run before
and after the the the prompt are inserted, respectively.  BEFORE
can also be a string in which case it is inserted via
`sly-insert-note' followed by the saved values' presentations.
REPL is the REPL buffer to return the objects to."
  (sly-eval-async
   `(slynk-mrepl:globally-save-object ',(car slyfun-and-args)
                                      ,@(cdr slyfun-and-args))
   #'(lambda (_ignored)
       (sly-mrepl--with-repl (or repl (sly-mrepl--find-create (sly-connection)))
         (sly-mrepl--copy-objects-to-repl nil before after)))))

(defun sly-mrepl--insert-call (spec results)
  (when (<= (point) (sly-mrepl--mark))
    (goto-char (point-max)))
  (insert (format "%s"
                  `(,spec
                    ,@(cl-loop for o in results
                               for i from 0
                               collect (make-symbol (format "#v%d:%d"
                                                            (cadr o)
                                                            i)))))))

(defun sly-mrepl--assert-mrepl ()
  (unless (eq major-mode 'sly-mrepl-mode)
    (sly-error "Not in a mREPL buffer")))


;;; ELI-like history (and a bugfix)
;;;
;;;
(defcustom sly-mrepl-eli-like-history-navigation nil
  "If non-NIL navigate history like ELI.
When this option is active, previous history entries navigated to
by M-p and M-n keep the current input and use it to surround the
history entry navigated to."
  :type 'boolean
  :group 'sly)

(defvar sly-mrepl--eli-input nil)

(defun sly-mrepl--set-eli-input ()
  (setq sly-mrepl--eli-input
        (and sly-mrepl-eli-like-history-navigation
             (let* ((offset (- (point) (sly-mrepl--mark)))
                    (existing (and (> offset 0)
                                   (buffer-substring (sly-mrepl--mark) (point-max)))))
               (when existing
                 (cons (substring existing 0 offset)
                       (substring existing offset)))))))

(defun sly-mrepl--keep-eli-input-maybe ()
  (when sly-mrepl--eli-input
    (save-excursion
      (goto-char (sly-mrepl--mark))
      (insert (car sly-mrepl--eli-input))
      (goto-char (point-max))
      (insert (cdr sly-mrepl--eli-input)))))

(defvar sly-mrepl--eli-input-overlay nil)

(defun sly-mrepl--surround-with-eli-input-overlay ()
  (if sly-mrepl--eli-input-overlay
      (move-overlay sly-mrepl--eli-input-overlay
                    (sly-mrepl--mark) (point-max))
    (setq sly-mrepl--eli-input-overlay
          (make-overlay (sly-mrepl--mark) (point-max))))
  (overlay-put sly-mrepl--eli-input-overlay
               'before-string (car sly-mrepl--eli-input))
  (overlay-put sly-mrepl--eli-input-overlay
               'after-string (cdr sly-mrepl--eli-input)))

(defun sly-mrepl--setup-comint-isearch ()
  ;; Defeat Emacs bug 19572 in Emacs whereby comint refuses to
  ;; i-search multi-line history entries. The doc of
  ;; `isearch-search-fun-function' should explain the need for this
  ;; lambda madness.
  ;;
  (unless (eq isearch-search-fun-function
              'isearch-search-fun-default)
    (set (make-local-variable 'isearch-search-fun-function)
         #'(lambda ()
             #'(lambda (&rest args)
                 (cl-letf
                     (((symbol-function
                        'comint-line-beginning-position)
                       #'field-beginning))
                   (apply (comint-history-isearch-search)
                          args))))))
  (sly-mrepl--set-eli-input)
  (when sly-mrepl-eli-like-history-navigation
    (set (make-local-variable 'isearch-push-state-function)
         #'sly-mrepl--isearch-push-state)))

(defun sly-mrepl--isearch-push-state (&rest args)
  (apply #'comint-history-isearch-push-state args)
  (unless (memq this-command
                '(isearch-backward isearch-forward))
    (sly-mrepl--surround-with-eli-input-overlay)))

(defun sly-mrepl--teardown-comint-isearch ()
  (set (make-local-variable 'isearch-search-fun-function)
       'isearch-search-fun-default)
  (when (overlayp sly-mrepl--eli-input-overlay)
    (delete-overlay sly-mrepl--eli-input-overlay)
    (setq sly-mrepl--eli-input-overlay nil))
  (sly-mrepl--keep-eli-input-maybe))


;;; Interactive commands
;;;
(defun sly-mrepl-indent-and-complete-symbol (arg)
  "Indent the current line, perform symbol completion or show arglist.
Completion performed by `completion-at-point'.  If there's no
symbol at the point, show the arglist for the most recently
enclosed macro or function."
  (interactive "P")
  (let ((pos (point)))
    (indent-for-tab-command arg)
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (completion-at-point))
            ((memq (char-before) '(?\t ?\ ))
             (sly-show-arglist))))))

(defun sly-mrepl-return (&optional end-of-input)
  "If the input is a whole expression, evaluate it and return the result."
  (interactive "P")
  (cl-assert (sly-connection))
  (cl-assert (process-live-p (sly-mrepl--process)) nil
             "No local live process, cannot use this REPL")
  (accept-process-output)
  (cond ((and
          (not sly-mrepl--read-mode)
          (sly-mrepl--busy-p))
         (sly-message "REPL is busy"))
        ((and (not sly-mrepl--read-mode)
              (or (sly-input-complete-p (sly-mrepl--mark) (point-max))
                  end-of-input))
         (sly-mrepl--send-input-sexp)
         (sly-mrepl--catch-up))
        (sly-mrepl--read-mode
         (unless end-of-input
           (goto-char (point-max))
           (newline))
         (let ((comint-input-filter (lambda (_s) nil)))
           (comint-send-input 'no-newline)))
        (t
         (newline-and-indent)
         (sly-message "Input not complete"))))

(defun sly-mrepl-previous-input-or-button (n)
  (interactive "p")
  (if (>= (point) (sly-mrepl--mark))
      (progn
        (unless (memq last-command
                      '(sly-mrepl-previous-input-or-button
                        sly-mrepl-next-input-or-button))
            (sly-mrepl--set-eli-input))
        (comint-previous-input n)
        (sly-mrepl--keep-eli-input-maybe))
    (sly-button-backward n)))

(defun sly-mrepl-next-input-or-button (n)
  (interactive "p")
  (sly-mrepl-previous-input-or-button (- n)))

(defun sly-mrepl (&optional pop-to-buffer)
  "Find or create the first useful REPL for the default connection."
  (interactive (list t))
  (let* ((buffer
          (sly-mrepl--find-create (sly-current-connection))))
    (when pop-to-buffer
      (pop-to-buffer buffer))
    buffer))

(defun sly-mrepl-on-connection ()
  (let* ((inferior-buffer (and (sly-process) (process-buffer (sly-process))))
         (inferior-window (and inferior-buffer (get-buffer-window inferior-buffer t))))
    (let ((sly-mrepl-pop-sylvester (or (eq sly-mrepl-pop-sylvester 'on-connection)
                                       sly-mrepl-pop-sylvester)))
      (sly-mrepl 'pop-to-buffer))
    (when inferior-window
      (bury-buffer inferior-buffer)
      (delete-window inferior-window))
    (goto-char (point-max))))

(defun sly-mrepl-new (connection &optional handle)
  "Create and setup a new REPL buffer for CONNECTION.
CONNECTION defaults to the current SLY connection.  If such a
buffer already exists, or a prefix arg is given, prompt for a
handle to distinguish the new buffer from the existing."
  (interactive
   ;; FIXME: Notice a subtle bug/feature than when calling
   ;; interactively in a buffer which has a connection, but not the
   ;; default connection, the new REPL will be for that connection.
   (let ((connection (sly-connection)))
     (list connection
           (if (or (get-buffer (sly-mrepl--buffer-name connection))
                   current-prefix-arg)
               (sly-read-from-minibuffer
                "Nickname for this new REPL? ")))))
  (let* ((name (sly-mrepl--buffer-name connection handle))
         (existing (get-buffer name)))
    (when (and handle existing)
      (sly-error "A REPL with that handle already exists"))
    ;; Take this oportunity to save any other REPL histories so that
    ;; the new REPL will see them.
    (sly-mrepl--save-all-histories)
    (let* ((local (sly-make-channel sly-listener-channel-methods))
           (buffer (pop-to-buffer name)))
      (with-current-buffer buffer
        (sly-mrepl-mode)
        (when (and (not existing)
                   (eq sly-mrepl-pop-sylvester t))
          (sly-mrepl--insert-note
           (concat "\n" (sly-mrepl-random-sylvester) "\n\n")
           'sly-mrepl-output-face))
        (setq sly-buffer-connection connection)
        (start-process (format "sly-pty-%s-%s"
                               (process-get connection
                                            'sly--net-connect-counter)
                               (sly-channel.id local))
                       (current-buffer)
                       nil)
        (set-process-query-on-exit-flag (sly-mrepl--process) nil)
        (setq header-line-format (format "Waiting for REPL creation ack for channel %d..."
                                         (sly-channel.id local)))
        (sly-channel-put local 'buffer (current-buffer))
        (add-hook 'kill-buffer-hook 'sly-mrepl--teardown nil 'local)
        (set (make-local-variable 'sly-mrepl--local-channel) local))
      (sly-eval-async
          `(slynk-mrepl:create-mrepl ,(sly-channel.id local))
        (lambda (result)
          (cl-destructuring-bind (remote thread-id) result
            (with-current-buffer buffer
              (sly-mrepl--read-input-ring)
              (setq header-line-format nil)
              (setq sly-current-thread thread-id)
              (set (make-local-variable 'sly-mrepl--remote-channel) remote)
              (unwind-protect
                  (run-hooks 'sly-mrepl-hook 'sly-mrepl-runonce-hook)
                (set-default 'sly-mrepl-runonce-hook nil))))))
      buffer)))

(defun sly-mrepl-insert-input (pos)
  (interactive (list (if (mouse-event-p last-input-event)
                         (posn-point (event-end last-input-event))
                       (point))))
  (sly-mrepl--assert-mrepl)
  (let* ((pos (if (eq (field-at-pos pos) 'sly-mrepl-input)
                  pos
                (1+ pos)))
         (new-input (and
                     (eq (field-at-pos (1+ pos)) 'sly-mrepl-input)
                     (field-string-no-properties pos)))
         (offset (and new-input
                      (- (point) (field-beginning pos)))))
    (cond (new-input
           (goto-char (sly-mrepl--mark))
           (delete-region (point) (point-max))
           (insert (sly-trim-whitespace new-input))
           (goto-char (+ (sly-mrepl--mark) offset)))
          (t
           (sly-error "No input at point")))))

(defun sly-mrepl-guess-package (&optional point interactive)
  (interactive (list (point) t))
  (let* ((point (or point (point)))
         (probe
          (previous-single-property-change point
                                           'sly-mrepl--prompt))
         (package (and probe
                       (or (get-text-property probe 'sly-mrepl--prompt)
                           (get-text-property (previous-single-property-change
                                               probe 'sly-mrepl--prompt)
                                              'sly-mrepl--prompt)))))
    (when interactive
      (sly-message "Guessed package \"%s\"" package))
    package))

(define-obsolete-function-alias 'sly-mrepl-sync-package-and-default-directory 'sly-mrepl-sync
  "1.0.0-alpha-3")

(defun sly-mrepl-sync (&optional package directory expression)
  "Go to the REPL, and set Slynk's PACKAGE and DIRECTORY.
Also yank EXPRESSION into the prompt.  Interactively gather
PACKAGE and DIRECTORY these values from the current buffer, if
available. In this scenario EXPRESSION is only set if a C-u
prefix argument is given."
  (interactive (list (sly-current-package)
                     (and buffer-file-name
                          default-directory)
                     (and current-prefix-arg
                          (sly-last-expression))))
  (sly-mrepl--with-repl (sly-mrepl--find-create (sly-connection))
    (when directory
      (cd directory))
    (sly-mrepl--eval-for-repl
     `(slynk-mrepl:sync-package-and-default-directory
       :package-name ,package
       :directory ,directory)
     :insert-p nil
     :before-prompt
     #'(lambda (results)
         (cl-destructuring-bind (package-2 directory-2) results
           (sly-mrepl--insert-note
            (cond ((and package directory)
                   (format "Synched package to %s and directory to %s" package-2 directory-2))
                  (directory
                   (format "Synched directory to %s" directory-2))
                  (package
                   (format "Synched package to %s" package-2))
                  (t
                   (format "Remaining in package %s and directory %s"
                           package-2 directory-2))))))
     :after-prompt
     #'(lambda (_results)
         (when expression
           (goto-char (point-max))
           (let ((saved (point)))
             (insert expression)
             (when (string-match "\n" expression)
               (indent-region saved (point-max)))))))))

(defun sly-mrepl-clear-repl ()
  "Clear all this REPL's output history.
Doesn't clear input history."
  (interactive)
  (sly-mrepl--assert-mrepl)
  (sly-mrepl--send `(:clear-repl-history)))

(defun sly-mrepl-clear-recent-output ()
  "Clear this REPL's output between current and last prompt."
  (interactive)
  (sly-mrepl--assert-mrepl)
  (cl-loop for search-start =
           (set-marker (make-marker)
                       (1+ (overlay-start sly-mrepl--last-prompt-overlay))) then pos
           for pos = (set-marker search-start
                                 (previous-single-property-change search-start 'field))
           while (and (marker-position pos)
                      ;; FIXME: fragile (1- pos), use narrowing
                      (not (get-text-property (1- pos) 'sly-mrepl--prompt))
                      (> pos (point-min)))
           when (eq (field-at-pos pos) 'sly-mrepl--output)
           do (let ((inhibit-read-only t))
                (delete-region (field-beginning pos)
                               (1+ (field-end pos)))))
  (sly-mrepl--insert-note (propertize "Cleared recent output"
                                      'sly-mrepl-break-output t))
  (sly-message "Cleared recent output"))


;;; "External" non-interactive functions for plugging into
;;; other parts of SLY
;;;
(defun sly-inspector-copy-part-to-repl (number)
  "Evaluate the inspector slot at point via the REPL (to set `*')."
  (sly-mrepl--save-and-copy-for-repl
   `(slynk:inspector-nth-part-or-lose ,number)
   :before (format "Returning inspector slot %s" number)))

(defun sly-db-copy-part-to-repl (frame-id var-id)
  "Evaluate the frame var at point via the REPL (to set `*')."
  (sly-mrepl--save-and-copy-for-repl
   `(slynk-backend:frame-var-value ,frame-id ,var-id)
   :repl (sly-mrepl--find-buffer (sly-current-connection) sly-current-thread)
   :before (format "Returning var %s of frame %s" var-id frame-id)))

(defun sly-apropos-copy-symbol-to-repl (name _type)
  (sly-mrepl--save-and-copy-for-repl
   `(common-lisp:identity ',(car (read-from-string name)))
   :before (format "Returning symbol %s" name)))

(defun sly-trace-dialog-copy-part-to-repl (id part-id type)
  "Eval the Trace Dialog entry under point in the REPL (to set *)"
  (sly-mrepl--save-and-copy-for-repl
   `(slynk-trace-dialog:trace-part-or-lose ,id ,part-id ,type)
   :before (format "Returning part %s (%s) of trace entry %s" part-id type id)))

(defun sly-db-copy-call-to-repl (frame-id spec)
  (sly-mrepl--save-and-copy-for-repl
   `(slynk-backend:frame-arguments ,frame-id)
   :before (format "The actual arguments passed to frame %s" frame-id)
   :after #'(lambda (objects)
              (sly-mrepl--insert-call spec objects))))

(defun sly-trace-dialog-copy-call-to-repl (trace-id spec)
  (sly-mrepl--save-and-copy-for-repl
   `(slynk-trace-dialog:trace-arguments-or-lose ,trace-id)
   :before (format "The actual arguments passed to trace %s" trace-id)
   :after #'(lambda (objects)
              (sly-mrepl--insert-call spec objects))))

(defun sly-mrepl-inside-string-or-comment-p ()
  (let ((mark (and (process-live-p (sly-mrepl--process))
                   (sly-mrepl--mark))))
    (when (and mark (> (point) mark))
      (let ((ppss (parse-partial-sexp mark (point))))
        (or (nth 3 ppss) (nth 4 ppss))))))


;;; The comma shortcut
;;;
(defun sly-mrepl-reset-shortcut (key-sequence)
  "Set `sly-mrepl-shortcut' and reset REPL keymap accordingly."
  (interactive "kNew shortcut key sequence? ")
  (when (boundp 'sly-mrepl-shortcut)
    (define-key sly-mrepl-mode-map sly-mrepl-shortcut nil))
  (set-default 'sly-mrepl-shortcut key-sequence)
  (define-key sly-mrepl-mode-map key-sequence
    '(menu-item "" sly-mrepl-shortcut
                :filter (lambda (cmd)
                          (if (and (eq major-mode 'sly-mrepl-mode)
                                   (sly-mrepl--shortcut-location-p))
                              cmd)))))

(defcustom sly-mrepl-shortcut (kbd ",")
  "Keybinding string used for the REPL shortcut commands.
When setting this variable outside of the Customize interface,
`sly-mrepl-reset-shortcut' must be used."
  :group 'sly
  :type 'key-sequence
  :set (lambda (_sym value)
         (sly-mrepl-reset-shortcut value)))

(defun sly-mrepl--shortcut-location-p ()
  (or (< (point) (sly-mrepl--mark))
      (and (not (let ((state (syntax-ppss)))
                  (or (nth 3 state) (nth 4 state))))
           (or (not (equal sly-mrepl-shortcut ","))
               (not (save-excursion
                      (search-backward "`" (sly-mrepl--mark) 'noerror)))))))

(defvar sly-mrepl-shortcut-alist
  '(("sayoonara" . sly-quit-lisp)
    ("disconnect" . sly-disconnect)
    ("disconnect all" . sly-disconnect-all)
    ("restart lisp" . sly-restart-inferior-lisp)
    ("set package" . sly-mrepl-set-package)
    ("set directory" . sly-mrepl-set-directory)
    ("clear repl" . sly-mrepl-clear-repl)))

(defun sly-mrepl-set-package ()
  (interactive)
  (let ((package (sly-read-package-name "New package: ")))
    (sly-mrepl--eval-for-repl `(slynk-mrepl:guess-and-set-package ,package))))

(defun sly-mrepl-set-directory ()
  (interactive)
  (let ((directory (read-directory-name "New directory: " default-directory nil t)))
    (sly-mrepl--save-and-copy-for-repl
     `(slynk:set-default-directory ,directory)
     :before (format "Setting directory to %s" directory))
    (cd directory)))

(defun sly-mrepl-shortcut ()
  (interactive)
  (let* ((string (sly-completing-read "Command: "
                                     (mapcar #'car sly-mrepl-shortcut-alist)
                                     nil
                                     'require-match))
         (command (and string
                       (cdr (assoc string sly-mrepl-shortcut-alist)))))
    (call-interactively command)))


;;; Backreference highlighting
;;;
(defvar sly-mrepl--backreference-overlays nil
  "List of overlays on top of REPL result buttons.")
(make-variable-buffer-local 'sly-mrepl--backreference-overlays)

(defun sly-mrepl-highlight-results (&optional entry-idx value-idx)
  "Highlight REPL results for ENTRY-IDX and VALUE-IDX.
If VALUE-IDX is nil or `all', highlight all results for entry
ENTRY-IDX.  If ENTRY-IDX is nil, highlight all results.  Returns
a list of result buttons thus highlighted"
  (interactive)
  (cl-loop
   for button in (sly-button-buttons-in (point-min) (point-max))
   for e-idx = (car (button-get button 'part-args))
   for v-idx = (cadr (button-get button 'part-args))
   when (and (button-type-subtype-p (button-type button) 'sly-mrepl-part)
             (not (button-get button 'sly-mrepl--highlight-overlay))
             (and (or (not entry-idx)
                      (= e-idx entry-idx))
                  (or (not value-idx)
                      (eq value-idx 'all)
                      (= v-idx value-idx))))
   collect button and
   do (let ((overlay (make-overlay (button-start button) (button-end button))))
        (push overlay sly-mrepl--backreference-overlays)
        (overlay-put overlay 'before-string
                     (concat
                      (propertize
                       (format "%s:%s"
                               (car (button-get button 'part-args))
                               (cadr (button-get button 'part-args)))
                       'face 'highlight)
                      " ")))))

(defun sly-mrepl-unhighlight-results ()
  "Unhighlight all repl results"
  (interactive)
  (mapc #'delete-overlay sly-mrepl--backreference-overlays)
  (setq sly-mrepl--backreference-overlays nil))

(defvar sly-mrepl--backreference-overlay nil)
(defvar sly-mrepl--backreference-prefix "#v")

(defun sly-mrepl--highlight-backreferences-maybe ()
  "Intended to be placed in `post-command-hook'."
  (sly-mrepl-unhighlight-results)
  (when sly-mrepl--backreference-overlay
    (delete-overlay sly-mrepl--backreference-overlay))
  (let* ((match (save-excursion
                  (sly-beginning-of-symbol)
                  (looking-at
                   (format "%s\\([[:digit:]]+\\)?\\(:\\([[:digit:]]+\\)\\|:\\)?"
                           sly-mrepl--backreference-prefix))))
         (m0 (and match (match-string 0)))
         (m1 (and m0 (match-string 1)))
         (m2 (and m1 (match-string 2)))
         (m3 (and m2 (match-string 3)))
         (entry-idx (and m1 (string-to-number m1))) 
         (value-idx (and match
                         (or (and m3 (string-to-number m3))
                             (and (not m2)
                                  'all)))))
    (when match
      (let ((buttons (sly-mrepl-highlight-results entry-idx value-idx))
            (overlay (or sly-mrepl--backreference-overlay
                         (set (make-local-variable 'sly-mrepl--backreference-overlay)
                              (make-overlay 0 0))))
            (message-log-max nil))
        (move-overlay sly-mrepl--backreference-overlay
                      (match-beginning 0) (match-end 0))
        (cond ((null buttons)
               (overlay-put overlay 'face 'font-lock-warning-face)
               (sly-message "No history references for backreference `%s'" m0))
              ((and buttons
                    entry-idx
                    value-idx)
               (overlay-put overlay 'face 'sly-action-face)
               (let* ((prefix (if (numberp value-idx)
                                  (format "Matched history value %s of entry%s: "
                                               value-idx
                                               entry-idx)
                                (format "Matched history entry %s%s: "
                                             entry-idx
                                             (if (rest buttons)
                                                 (format " (%s values)" (length buttons))
                                               ""))))
                      (hint (propertize
                             (truncate-string-to-width
                              (replace-regexp-in-string "\n" " " (button-label (first buttons)))
                              (- (window-width (minibuffer-window))
                                 (length prefix) 10)
                              nil
                              nil
                              "...")
                             'face
                             'sly-action-face)))
                 (sly-message (format "%s%s" prefix hint))))
              (buttons
               (sly-message "Ambiguous backreference `%s', %s values possible"
                            m0 (length buttons))
               (overlay-put overlay 'face 'font-lock-warning-face))
              (t
               (overlay-put overlay 'face 'font-lock-warning-face)
               (sly-message "Invalid backreference `%s'" m0)))))))


;;;; Menu
;;;;
(easy-menu-define sly-mrepl--shortcut-menu nil
  "Menu for accessing the mREPL anywhere in sly."
  (let* ((C '(sly-connected-p)))
    `("mREPL"
      ["Go to default REPL" sly-mrepl ,C]
      ["New REPL" sly-mrepl-new ,C]
      ["Sync Package & Directory" sly-mrepl-sync
       (and sly-editing-mode ,C)])))

(easy-menu-add-item sly-menu nil sly-mrepl--shortcut-menu "Documentation")

(easy-menu-define sly-mrepl--menu sly-mrepl-mode-map
  "Menu for SLY's MREPL"
  (let* ((C '(sly-connected-p)))
    `("SLY-mREPL"
      [ " Complete symbol at point " sly-mrepl-indent-and-complete-symbol ,C ]
      [ " Interrupt " sly-interrupt ,C ]
      [ " Isearch history backward " isearch-backward ,C]
      "----"
      [ " Clear REPL" sly-mrepl-clear-repl ,C ]
      [ " Clear last output" sly-mrepl-clear-recent-output ,C ])))


(defvar sly-mrepl--debug-overlays nil)

(defun sly-mrepl--debug (&rest ignored)
  (interactive)
  (mapc #'delete-overlay sly-mrepl--debug-overlays)
  (let ((overlay (make-overlay sly-mrepl--output-mark
                               (sly-mrepl--mark)))
        (color (if (< sly-mrepl--output-mark (sly-mrepl--mark))
                   "green"
                 "orange"))
        (marker-color (if (= sly-mrepl--output-mark (sly-mrepl--mark))
                          "red"
                        "purple")))
    (overlay-put overlay
                 'face `(:background ,color))
    (overlay-put overlay
                 'after-string (propertize "F" 'face `(:background ,marker-color)))
    (push overlay sly-mrepl--debug-overlays)))

(defun sly-mrepl--turn-on-debug ()
  (interactive)
  (add-hook 'after-change-functions 'sly-mrepl--debug nil 'local)
  (add-hook 'post-command-hook 'sly-mrepl--debug nil 'local))

(defun sly-mrepl--turn-off-debug ()
  (interactive)
  (remove-hook 'after-change-functions 'sly-mrepl--debug 'local)
  (remove-hook 'post-command-hook 'sly-mrepl--debug 'local))


;;; Sylvesters
;;;
(defvar  sly-mrepl--sylvesters
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name "sylvesters.txt"
                       (file-name-directory load-file-name)))
    (cl-loop while (< (point) (point-max))
             for start = (point)
             do (search-forward "\n\n" nil 'noerror)
             collect (buffer-substring-no-properties start (- (point) 2)))))

(defun sly-mrepl-random-sylvester ()
  (let* ((sylvester (nth (random (length sly-mrepl--sylvesters))
                         sly-mrepl--sylvesters))
         (woe (sly-random-words-of-encouragement))
         (uncommented
          (replace-regexp-in-string "@@@@" woe sylvester)))
    uncommented))

(provide 'sly-mrepl)
