(require 'sly-parse)
(require 'sly-tests "lib/sly-tests")

(def-sly-test form-up-to-point.1
    (buffer-sexpr result-form &optional skip-trailing-test-p)
    ""
    '(("(char= #\\(*HERE*"
       ("char=" "#\\(" slynk::%cursor-marker%))
      ("(char= #\\( *HERE*"
       ("char=" "#\\(" "" slynk::%cursor-marker%))
      ("(char= #\\) *HERE*"
       ("char=" "#\\)" "" slynk::%cursor-marker%))
      ("(char= #\\*HERE*"
       ("char=" "#\\" slynk::%cursor-marker%) t)
      ("(defun*HERE*"
       ("defun" slynk::%cursor-marker%))
      ("(defun foo*HERE*"
       ("defun" "foo" slynk::%cursor-marker%))
      ("(defun foo (x y)*HERE*"
       ("defun" "foo"
	("x" "y") slynk::%cursor-marker%))
      ("(defun foo (x y*HERE*"
       ("defun" "foo"
	("x" "y" slynk::%cursor-marker%)))
      ("(apply 'foo*HERE*"
       ("apply" "'foo" slynk::%cursor-marker%))
      ("(apply #'foo*HERE*"
       ("apply" "#'foo" slynk::%cursor-marker%))
      ("(declare ((vector bit *HERE*"
       ("declare" (("vector" "bit" "" slynk::%cursor-marker%))))
      ("(with-open-file (*HERE*"
       ("with-open-file" ("" slynk::%cursor-marker%)))
      ("(((*HERE*"
       ((("" slynk::%cursor-marker%))))
      ("(defun #| foo #| *HERE*"
       ("defun" "" slynk::%cursor-marker%))
      ("(defun #-(and) (bar) f*HERE*"
       ("defun" "f" slynk::%cursor-marker%))
      ("(remove-if #'(lambda (x)*HERE*"
       ("remove-if" ("lambda" ("x") slynk::%cursor-marker%)))
      ("`(remove-if ,(lambda (x)*HERE*"
       ("remove-if" ("lambda" ("x") slynk::%cursor-marker%)))
      ("`(remove-if ,@(lambda (x)*HERE*"
       ("remove-if" ("lambda" ("x") slynk::%cursor-marker%))))
  (sly-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (should (equal result-form
                   (sly-parse-form-upto-point 10)))
    (unless skip-trailing-test-p
      (insert ")") (backward-char)
      (should (equal result-form
                     (sly-parse-form-upto-point 10))))))

(provide 'sly-parse-tests)
