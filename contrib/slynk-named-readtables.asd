;;; -*- lisp -*-
(in-package :asdf)

(defsystem :slynk-named-readtables
    :author "João Távora <https://github.com/capitaomorte>"
    :depends-on (#:slynk)
  :description "NAMED-READTABLES support for Slynk"
  :components ((:file "slynk-named-readtables")))

