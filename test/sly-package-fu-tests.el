;; -*- lexical-binding: t; -*-
(require 'sly-package-fu "contrib/sly-package-fu")
(require 'sly-tests "lib/sly-tests")

(def-sly-test package-fu-import (initial final symbol-to-import)
  "Check if importing `import` on `initial-defpackage` results in `final-defpackage."
  '((((defpackage :foo) (in-package :foo))
     ((defpackage :foo  (:import-from :cl :find)) (in-package :foo))
     cl:find)
    (((defpackage :foo  (:import-from :cl :find)) (in-package :foo))
     ((defpackage :foo  (:import-from :cl :position :find)) (in-package :foo))
     cl:position)
    (((defpackage :foo (:import-from :bknr.datastore-dummy :find)) (in-package :foo))
     ((defpackage :foo (:import-from :bknr.datastore-dummy
                                     :position
                                     :find))
      (in-package :foo))
     bknr.datastore-dummy::position)
    (((defpackage :foo
        (:import-from :bar/car :find))
      (in-package :foo))
     ((defpackage :foo
        (:import-from :bar/car :find)
        (:import-from :bar :position))
      (in-package :foo))
     bar::position)
    (((defpackage :foo
        (:import-from :bar))
      (in-package :foo))
     ((defpackage :foo
        (:import-from :bar :position))
      (in-package :foo))
     bar::position))
  (let ((file (make-temp-file "sly-package-fu--fixture"))
        (sly-export-symbol-representation-auto nil)
        (sly-export-symbol-representation-function
         ;; For the purpose of these tests, reading back the
         ;; #:.. style symbols breaks equality, since they are
         ;; uninterned.
         (lambda (n) (format ":%s" n))))
    (with-temp-buffer
      (find-file file)
      (lisp-mode)
      (setq indent-tabs-mode nil)
      (dolist (f initial) (insert (pp-to-string f)))
      ;; FIXME: using internal implementation detail
      (sly-package-fu--add-or-update-import-from-form
       (pp-to-string symbol-to-import))

      (should (equal final
                     (cl-loop initially (goto-char (point-min))
                              for f = (ignore-errors (read (current-buffer)))
                              while f collect f))))))

(define-sly-ert-test package-fu-jumps-to-import-from-happy-path ()
  (with-temp-buffer
    (insert "(defpackage :foo (:import-from #:bar #:car))")
    ;; Expect to be here:........................^
    (goto-char (point-min))
    (sly-package-fu--search-import-from "bar")
    (should (equal 37 (point)))))

(define-sly-ert-test package-fu-jumps-to-import-from-with-new-line ()
  (with-temp-buffer
    (insert "(defpackage :foo (:import-from #:bar\n#:car))")
    ;; Expect to be here:........................^
    (goto-char (point-min))
    (sly-package-fu--search-import-from "bar")
    (should (equal 37 (point)))))

(define-sly-ert-test package-fu-does-not-jump-to-package-prefix ()
  (with-temp-buffer
    (insert "(defpackage :foo (:import-from #:barcelona\n#:car))")
    (goto-char (point-min))
    (should
     (equal nil
            (sly-package-fu--search-import-from "bar")))
    (should (equal 1 (point)))))

(define-sly-ert-test package-fu-jump-to-end-even-when-closing-parenthesis ()
  (with-temp-buffer
    (insert "(defpackage :foo (:import-from #:bar))")
    ;; Expect to be here:........................^
    (goto-char (point-min))
    (sly-package-fu--search-import-from "bar")
    (should (equal 37 (point)))))


(provide 'sly-package-fu-tests)
