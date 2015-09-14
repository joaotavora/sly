(require 'sly-fuzzy)
(require 'sly-tests "lib/sly-tests")
(require 'cl-lib)

(def-sly-test minimum-required-completions
    (prefix required-completions partial)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" ("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                     "cl:compiled-function" "cl:compiled-function-p" 
                     "cl:compiler-macro" "cl:compiler-macro-function")
       "cl:compile")
      ("cl:foobar" nil nil)
      ("slynk::compile-file" ("slynk::compile-file" 
                              "slynk::compile-file-for-emacs"
                              "slynk::compile-file-if-needed"
                              "slynk::compile-file-output"
                              "slynk::compile-file-pathname")
       "slynk::compile-file")
      ("cl:m-v-l" ("cl:multiple-value-list" "cl:multiple-values-limit") "cl:multiple-value")
      ("common-lisp" ("common-lisp-user:" "common-lisp:") "common-lisp"))
  (let ((completions (sly-fuzzy-completions prefix)))
    (cl-loop for required in required-completions
             unless (cl-loop for suggested in (car completions)
                             for i below 30
                             when (string= (car suggested) required) return it)
             do (ert-fail (format "Expected to find \"%s\" in the group of %d first suggestions for completing \"%s\""
                                  required
                                  (length head)
                                  prefix)))))

(provide 'sly-fuzzy-tests)
