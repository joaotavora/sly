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
     bknr.datastore-dummy::position))
  (let ((file (make-temp-file "sly-package-fu--fixture")))
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



(provide 'sly-package-fu-tests)
