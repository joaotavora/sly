;; -*- lexical-binding: t; -*-
(require 'sly-package-fu "contrib/sly-package-fu")
(require 'sly-tests "lib/sly-tests")

(def-sly-test sly-package-fu-sly-import (initial-defpackage final-defpackage symbol)
  "Check if importing `import` on `initial-defpackage` results in `final-defpackage."
  '(("(defpackage :foo
  (:use :cl))
(in-package :foo)"
     "(defpackage :foo
  (:use :cl)
  (:import-from #:cl
                #:find))
(in-package :foo)"
     "cl:find")
    ("(defpackage :foo
  (:use :cl)
  (:import-from #:cl
                #:find))
(in-package :foo)"
     "(defpackage :foo
  (:use :cl)
  (:import-from #:cl
                #:position
                #:find))
(in-package :foo)"
     "cl:position")
    ("(defpackage :foo
  (:use :cl)
  (:import-from #:bknr.datastore-dummy
                #:find))
(in-package :foo)"
     "(defpackage :foo
  (:use :cl)
  (:import-from #:bknr.datastore-dummy
                #:position
                #:find))
(in-package :foo)"
     "bknr.datastore-dummy:position"))
  (let ((file (make-temp-file "sly-package-fu--fixture")))
    (with-temp-buffer
      (find-file file)
      (lisp-mode)
      (setq indent-tabs-mode nil)
      (insert initial-defpackage)
      (sly-package-fu--add-or-update-import-from-form symbol)
      (should (equal final-defpackage
                     (buffer-string))))))

(provide 'sly-package-fu-tests)
