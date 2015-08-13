(defpackage #:slynk-named-readtables (:use :cl #:slynk-api))
(in-package #:slynk-named-readtables)

(defvar *find-readtable-function* nil
  "Function taking a string designating a readtable.
The function should return a READTABLEP object")

(defun find-readtable-by-name (string)
  "Find a  readtable corresponding to STRING."
  (when string
    (if *find-readtable-function*
        (funcall *find-readtable-function* string)
        (let* ((find-readtable-fn (and (find-package :editor-hints.named-readtables)
                                       (find-symbol "FIND-READTABLE" :editor-hints.named-readtables)))
               (readtable-designator
                 (and find-readtable-fn
                      string
                      (with-buffer-syntax ()
                        (let ((*read-eval* nil))
                          ;; JT@15/08/13: Perhaps READ-FROM-STRING is
                          ;; questionable here...
                          (read-from-string string))))))
          (funcall find-readtable-fn readtable-designator)))))


(defun wrap-in-named-readtable (in-function &key (named-readtable nil)
                                &allow-other-keys)
  "Wrap IN-FUNCTION in readtable named by NAMED-READTABLE, a string."
  (let* ((guess (and named-readtable
                     (find-readtable-by-name named-readtable))))
    (lambda ()
      (with-buffer-syntax (nil guess)
        (funcall in-function)))))

(pushnew 'wrap-in-named-readtable *eval-for-emacs-wrappers*)

(provide 'slynk-named-readtables)
