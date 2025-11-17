;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((require-final-newline . t)
         (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))
 ("lib" .
  ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path . ("./" "../"))))))
 ("contrib" .
  ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path . ("./" "../"))))))
 ("test" .
  ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path . ("./" "../")))))))
