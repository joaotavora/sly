;; -*- lexical-binding: t; -*-
(require 'sly-mrepl)
(require 'sly-tests "lib/sly-tests")
(require 'cl-lib)
(require 'ert-x)


(cl-defun sly-mrepl-tests--assert-prompt (&optional (prompt "CL-USER>"))
  (let ((proper-prompt-p nil))
    (cl-loop 
     repeat 5
     when (looking-back (format "%s $" prompt) (- (point) 100))
     do (setq proper-prompt-p t)
     (cl-return)
     do (sit-for 0.3))
    (or proper-prompt-p
        (ert-fail (format "Proper prompt not seen in time (saw last 20 chars as \"%s\")"
                          (buffer-substring-no-properties (max (point-min)
                                                               (- (point-max)
                                                                  20))
                                                          (point-max)))))))

(defun sly-mrepl-tests--assert-dedicated-stream ()
  (let ((dedicated-stream nil))
    (cl-loop 
     repeat 5
     when (and sly-mrepl--dedicated-stream
               (processp sly-mrepl--dedicated-stream)
               (process-live-p sly-mrepl--dedicated-stream))
     do (setq dedicated-stream t)
     (cl-return)
     do (sleep-for 0 300))
    (or dedicated-stream
        (ert-fail "Dedicated stream not setup correctly"))))

(defvar sly-mrepl-tests--debug nil)
(setq sly-mrepl-tests--debug nil)

(defmacro sly-mrepl-tests--with-basic-repl-setup (&rest body)
  (declare (debug (&rest form)))
  `(let ((sly-buffer-package "COMMON-LISP-USER"))
     (with-current-buffer (sly-mrepl-new (sly-current-connection)
                                         "test-only-repl")
       (unwind-protect
           (progn
             (sly-mrepl-tests--assert-prompt)
             (sly-mrepl-tests--assert-dedicated-stream)
             ,@body)
         (unless sly-mrepl-tests--debug
           (kill-buffer (current-buffer)))))))

(defun sly-mrepl-tests--current-input-string ()
  (buffer-substring-no-properties (sly-mrepl--mark) (point-max)))

(define-sly-ert-test basic-repl-setup ()
  (sly-mrepl-tests--with-basic-repl-setup))

(define-sly-ert-test repl-values-and-button-navigation ()
  (sly-mrepl-tests--with-basic-repl-setup
   (insert "(values (list 1 2 3) #(1 2 3))")
   (sly-mrepl-return)
   (sly-mrepl-tests--assert-prompt)
   (ert-simulate-command '(sly-button-backward 1))
   (ert-simulate-command '(sly-button-backward 1))
   (should-error
    (ert-simulate-command '(sly-button-backward 1)))
   (ert-simulate-command '(sly-button-forward 1))))

(when (>= emacs-major-version 25)
  (define-sly-ert-test repl-completion-pop-up-window ()
    (sly-mrepl-tests--with-basic-repl-setup
     (insert "(setq echonumberli)")
     (backward-char 1)
     (ert-simulate-command '(completion-at-point))
     (should (get-buffer-window "*sly-completions*"))))

  (define-sly-ert-test repl-completion-choose-candidates ()
    (sly-mrepl-tests--with-basic-repl-setup
     (let ((symbol-snippet "multiple-value-t"))
       (insert "'()")
       (backward-char 1)
       (insert symbol-snippet)
       (ert-simulate-command '(completion-at-point))
       (should (get-buffer-window "*sly-completions*"))
       (ert-simulate-command '(sly-choose-completion))
       (should (string= "'(multiple-value-setq)"
                        (sly-mrepl-tests--current-input-string)))
       (backward-sexp)
       (kill-sexp)
       (insert symbol-snippet)
       (ert-simulate-command '(completion-at-point))
       (ert-simulate-command '(sly-next-completion 1))
       (ert-simulate-command '(sly-choose-completion))
       (should (string= "'(multiple-value-list)"
                        (sly-mrepl-tests--current-input-string)))))))

(provide 'sly-mrepl-tests)
