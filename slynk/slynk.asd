;;; -*- lisp -*-
(in-package :asdf)

;; ASDF system definition for loading the Slynk server independently
;; of Emacs.
;;
;; Usage:
;;
;;   (push #p"/path/to/this/file/" asdf:*central-registry*)
;;   (asdf:load-system :slynk)
;;   (slynk:create-server :port PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Slynk server is running on localhost:ACTUAL-PORT. You can
;; use `M-x sly-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

(defsystem :slynk
  :serial t
  ;; SBCL emits full WARNINGs during compilation if there is a DEFPACKAGE form
  ;; at variance with the current state of the package. Variance includes the
  ;; DEFPACKAGE's :EXPORT list being different than the current :EXPORT
  ;; list. Slynk uses CL:EXPORT a lot, resulting in the :SLYNK and
  ;; :SLYNK-BACKEND packages being in variance if the system is reloaded.
  ;;
  ;; Additionally, SBCL emits a (full) deprecation WARNING for FD->HANDLE at
  ;; compile time. (But only on Windows)
  ;;
  ;; The specification for COMPILE-FILE states that it must return a true
  ;; FAILURE-P value if there are any ERRORs *or* full WARNINGS. ASDF sees
  ;; this, and by default signals an ERROR instead of loading the fasl. ASDF
  ;; exports *COMPILE-FILE-FAILURE-BEHAVIOR* to control this behavior. However,
  ;; this variable is really meant for system consumers to set. If a system
  ;; knows that it produces WARNINGs and that they're acceptable, it's
  ;; preferred to provide an :AROUND-COMPILE hook that handles the warnings so
  ;; that COMPILE-FILE never returns failure.
  ;;
  ;; This hook handles all full WARNINGs produced during compilation by
  ;; printing them and then muffling them. It's a bit of a big hammer since it
  ;; muffles every full WARNING, but SBCL does not export specific warnings
  ;; that we can muffle. We may be able to do better by using
  ;; UIOP:DEFINE-PACKAGE to avoid package variance warnings altogether or
  ;; UIOP:MATCH-CONDITION-P to muffle only the warnings we want based on their
  ;; format string. That would ensure no new warnings slip in in the future,
  ;; but would require bumping the minimum ASDF version needed to run Slynk.
  #+sbcl
  :around-compile
  #+sbcl
  (lambda (thunk)
    (handler-bind (((and warning (not style-warning))
                     (lambda (c)
                       (format *error-output* "~&~@<~S: ~3i~:_~A~:>~%"
                               (class-name (class-of c)) c)
                       (muffle-warning c))))
      (let ((sb-ext:*on-package-variance* '(:warn t)))
        (funcall thunk))))
  :components
  ((:file "slynk-backend")
   ;; If/when we require ASDF3, we shall use :if-feature instead
   #+(or cmu sbcl scl)
   (:file "slynk-source-path-parser")
   #+(or cmu ecl sbcl scl)
   (:file "slynk-source-file-cache")
   #+clisp
   (:file "xref")
   #+(or clisp clozure clasp)
   (:file "metering")
   (:module "backend"
    :serial t
    :components (#+allegro
                 (:file "allegro")
                 #+armedbear
                 (:file "abcl")
                 #+clisp
                 (:file "clisp")
                 #+clozure
                 (:file "ccl")
                 #+cmu
                 (:file "cmucl")
                 #+cormanlisp
                 (:file "corman")
                 #+ecl
                 (:file "ecl")
                 #+lispworks
                 (:file "lispworks")
                 #+sbcl
                 (:file "sbcl")
                 #+clasp
                 (:file "clasp")
                 #+scl
                 (:file "scl")
                 #+mkcl
                 (:file "mkcl")))
   #-armedbear
   (:file "slynk-gray")
   (:file "slynk-match")
   (:file "slynk-rpc")
   (:file "slynk")
   (:file "slynk-completion")
   (:file "slynk-apropos")))

(defmethod perform :after ((o load-op) (c (eql (find-system :slynk))))
  (format *debug-io* "~&SLYNK's ASDF loader finished.")
  (funcall (read-from-string "slynk::init")))


;;; Contrib systems (should probably go into their own file one day)
;;;
(defsystem :slynk/arglists
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-arglists")))

(defsystem :slynk/fancy-inspector
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-fancy-inspector")))

(defsystem :slynk/package-fu
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-package-fu")))

(defsystem :slynk/mrepl
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-mrepl")))

(defsystem :slynk/trace-dialog
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-trace-dialog")))

(defsystem :slynk/profiler
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-profiler")))

(defsystem :slynk/stickers
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-stickers")))

(defsystem :slynk/indentation
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-indentation")))

(defsystem :slynk/retro
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-retro")))

