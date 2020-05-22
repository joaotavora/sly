;; -*- lexical-binding: t; -*-
(require 'sly-indentation)
(require 'sly-tests "lib/sly-tests")

(sly-define-common-lisp-style "common-lisp-indent-test"
    ;; Used to specify a few complex indentation specs for testing.
    ;; (:inherit "basic") ; Commented: unnecessatily messes up test 58
    (:indentation
     (complex-indent.1 ((&whole 4 (&whole 1 1 1 1 (&whole 1 1) &rest 1)
                                &body) &body))
     (complex-indent.2 (4 (&whole 4 &rest 1) &body))
     (complex-indent.3 (4 &body))))

(defun sly-indentation-mess-up-indentation ()
    (while (not (eobp))
      (forward-line 1)
      (unless (looking-at "^$")
        (cl-case (random 2)
          (0
           ;; Delete all leading whitespace -- except for
           ;; comment lines.
           (while (and (looking-at " ") (not (looking-at " ;")))
             (delete-char 1)))
          (1
           ;; Insert whitespace random.
           (let ((n (1+ (random 24))))
             (while (> n 0) (cl-decf n) (insert " ")))))))
    (buffer-string))

(defvar sly-indentation--test-function nil
  "Can be set indentation tests to `indent-region' if need be.")

(defun sly-indentation-test--1 (bindings expected)
  (cl-flet ((count-leading
             (line)
             (cl-loop for char across line
                      while (eq char ? )
                      count 1)))
    (with-temp-buffer
      (lisp-mode)
      (setq indent-tabs-mode nil)
      (sly-common-lisp-set-style "common-lisp-indent-test")
      (cl-loop for (sym value) in bindings
               do (set (make-local-variable sym) value))
      (insert expected)
      (goto-char (point-min))
      (let ((mess (sly-indentation-mess-up-indentation)))
        (when (string= mess expected)
          (ert-fail "Could not mess up indentation?"))
        (goto-char (point-min))
        (indent-region (point-min) (point-max)) ;; Used to be
                                                ;;  ‘indent-sexp’, but
                                                ;;  was super unstable
                                                ;;  on travis, for
                                                ;;  some reason.
        (delete-trailing-whitespace)
        (let ((expected-lines (split-string expected "\n"))
              (observed-lines (split-string (buffer-string) "\n")))
          (should (= (length expected-lines)
                     (length observed-lines)))
          (cl-loop for expected in expected-lines
                   for observed in observed-lines
                   for n-expected = (count-leading expected)
                   for n-observed = (count-leading observed)
                   unless (= n-expected n-observed)
                   do (message "Starting with this mess:\n%s" mess)
                   (message "\nGot this result:\n%s" (buffer-string))
                   (ert-fail
                       (format
                        "Expected line `%s' to have %d leading spaces. Got %d"
                        expected n-expected n-observed)))
          ;; (should (equal expected (buffer-string)))
          )))))

(eval-and-compile
  (defun sly-indentation-test-form (test-name bindings expected)
    `(define-sly-ert-test ,test-name ()
       ,(format "An indentation test named `%s'" test-name)
       (sly-indentation-test--1 ',bindings ,expected)))

  (defun sly-indentation-test-forms-for-file (file)
    (with-current-buffer
        (find-file-noselect (expand-file-name file sly-path))
      (goto-char (point-min))
      (cl-loop
       while (re-search-forward ";;; Test:[\t\n\s]*\\(.*\\)[\t\n\s]" nil t)
       for test-name = (intern (match-string-no-properties 1))
       for bindings =
       (save-restriction
         (narrow-to-region (point)
                           (progn (forward-comment
                                   (point-max))
                                  (point)))
         (save-excursion
           (goto-char (point-min))
           (cl-loop while
                    (re-search-forward
                     "\\([^\s]*\\)[\t\n\s]*:[\t\n\s]*\\(.*\\)[\t\n\s]" nil t)
                    collect (list
                             (intern (match-string-no-properties 1))
                             (car
                              (read-from-string (match-string-no-properties 2)))))))
       for expected = (buffer-substring-no-properties (point)
                                                      (scan-sexps (point)
                                                                  1))
       collect (sly-indentation-test-form test-name bindings expected)))))

(defmacro sly-indentation-define-tests ()
    `(progn
       ,@(sly-indentation-test-forms-for-file "test/sly-cl-indent-test.txt")))

(sly-indentation-define-tests)

(provide 'sly-indentation-tests)
