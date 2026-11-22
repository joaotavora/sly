;; -*- lexical-binding: t; -*-
(require 'sly-refactor "contrib/sly-refactor")
(require 'sly-tests "lib/sly-tests")

;; make sure we're always using the same indentation when running the tests
;; and restore the user's indentation settings afterwards
;; and save and restore the user's sly-threading-macro setting
(defmacro sly-thread-test (name description before after &rest body)
  (declare (indent 1))
  (let ((macro (gensym))
        (indent-settings (gensym))
        (macro-type (gensym))
        (macros ''(-> ->> ~> ~>>)))
    `(sly-refactor-test ,name ,description ,before ,after
                        (let ((,macro-type sly-threading-macro)
                              ,indent-settings)
                          (dolist (,macro ,macros)
                            (push (get ,macro 'sly-common-lisp-indent-function) ,indent-settings))
                          (dolist (,macro ,macros)
                            (put ,macro 'sly-common-lisp-indent-function 4))
                          (setq sly-threading-macro "->")
                          (unwind-protect
                              (progn ,@body)
                            (dolist (,macro (reverse ,macros))
                              (put ,macro 'sly-common-lisp-indent-function (pop
                                                                            ,indent-settings)))
                            (setq sly-threading-macro ,macro-type))))))

(sly-thread-test thread-first
  "Turn form into thread first style"
  "(f (g (h x) y) z)"
  "(-> x
    h
    (g y)
    (f z))"
  (sly-thread-first-all))

(sly-thread-test thread-last
  "Turn form into thread last style"
  "(f z (g y (h x)))"
  "(->> x
    h
    (g y)
    (f z))"
  (sly-thread-last-all))

(sly-thread-test thread-unwind
  "Unwind thread once."
  "(-> 1
    (+ 3)
    (* 4)
    (/ 7))"
  "(-> (+ 1 3)
    (* 4)
    (/ 7))"
  (sly-unwind))

(sly-thread-test thread-unwind-all
  "Fully unwind the threading macro."
  "(-> 1
    (+ 3)
    (* 4)
    (/ 7))"
  "(/ (* (+ 1 3) 4) 7)"
  (sly-unwind-all))

(sly-thread-test remember-line-joined
  "Make sure unwind restores newlines we got rid of when threading."
  "(f
 (g x)
 y)"
   "(f
 (g x)
 y)"
  (sly-thread-first-all)
  (sly-unwind-all))

(sly-thread-test remember-line-joined-thread-last
  "Make sure unwind restores newlines we got rid of when threading last."
  "(* 1
   (/ 2
      (+ 4
         5)))"
  "(* 1
   (/ 2
      (+ 4
         5)))"
  (sly-thread-last-all)
  (sly-unwind-all))
