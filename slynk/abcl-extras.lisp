

(in-package slynk/abcl)

;;; Have CL:INSPECT use SLY
;;;
;;; Since Slynk may also be run in a server not running under Emacs
;;; and potentially with other REPLs, we export a functional toggle
;;; for the user to call after loading these definitions.
(defun enable-cl-inspect-in-emacs ()
  (slynk::wrap 'cl:inspect :use-sly :replace 'slynk::inspect-in-emacs))

;; ??? repair bare print object so inspector titles show java class
(defun %print-unreadable-object-java-too (object stream type identity body)
  (setf stream (sys::out-synonym-of stream))
  (when *print-readably*
    (error 'print-not-readable :object object))
  (format stream "#<")
  (when type
    (if (java-object-p object)
        ;; Special handling for java objects
        (if (jinstance-of-p object "java.lang.Class")
            (progn
              (write-string "jclass " stream)
              (format stream "~a" (jclass-name object)))
            (format stream "~a" (jclass-name (jobject-class object))))
        ;; usual handling
        (format stream "~S" (type-of object)))
      (format stream " "))
  (when body
    (funcall body))
  (when identity
    (when (or body (not type))
      (format stream " "))
    (format stream "{~X}" (sys::identity-hash-code object)))
  (format stream ">")
  nil)


;;;; Debugger

;; Copied from slynk-sbcl.lisp.
#+abcl-introspect
(defvar sys::*caught-frames*)
;;
;; Notice that *INVOKE-DEBUGGER-HOOK* is tried before *DEBUGGER-HOOK*,
;; so we have to make sure that the latter gets run when it was
;; established locally by a user (i.e. changed meanwhile.)
(defun make-invoke-debugger-hook (hook)
  (lambda (condition old-hook)
    (prog1 (let (#+abcl-introspect
                 (sys::*caught-frames* nil))
             ;; the next might be the right thing for earlier lisps but I don't know
             ;;; XXX probably doesn't work in absence of ABCL-INTROSPECT on abcl-1.4 and earlier
             (let (#+abcl-introspect
                   (sys::*saved-backtrace*
                    (if (fboundp 'sys::new-backtrace)
                        (sys::new-backtrace condition)
                        (sys::backtrace))))
               (if *debugger-hook*
                   (funcall *debugger-hook* condition old-hook)
                   (funcall hook condition old-hook)))))))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (sys::*invoke-debugger-hook* (make-invoke-debugger-hook hook)))
    (funcall fun)))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setq sys::*invoke-debugger-hook* (make-invoke-debugger-hook function)))

(defvar *sldb-topframe*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* ((magic-token (intern "SLYNK-DEBUGGER-HOOK" 'slynk))
         (*sldb-topframe*
           (or
            (second (member magic-token
                            #+abcl-introspect sys::*saved-backtrace*
                            #-abcl-introspect (sys:backtrace)
                            :key (lambda (frame)
                                   (first (sys:frame-to-list frame)))))
            (car sys::*saved-backtrace*)))
         #+#.(slynk-backend:with-symbol *debug-condition* 'ext)
         (ext::*debug-condition* slynk::*slynk-debugger-condition*))
    (funcall debugger-loop-fn)))

(defun backtrace (start end)
  "A backtrace without initial SLYNK frames."
  (let ((backtrace
         #+abcl-introspect sys::*saved-backtrace*
         #-abcl-introspect (sys:backtrace)))
    (subseq (or (member *sldb-topframe* backtrace) backtrace) start end)))

(defun nth-frame (index)
  (nth index (backtrace 0 nil)))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (backtrace start end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Inspecting

;;; BEGIN FIXME move into generalized Slynk infrastructure, or add to contrib mechanism
;; this is only for hyperspec request in an inspector window
;; TODO have sly-hyperspec-lookup respect this variable too
(defvar *sly-inspector-hyperspec-in-browser* t
  "If t then invoking hyperspec within the inspector browses the hyperspec in an emacs buffer, otherwise respecting the value of browse-url-browser-function")

(defun hyperspec-do (name)
  (let ((form `(let ((browse-url-browser-function
                       ,(if *sly-inspector-hyperspec-in-browser*
                            '(lambda(a v) (eww a))
                            'browse-url-browser-function)))
                        (sly-hyperdoc-lookup ,name))))
    (slynk::eval-in-emacs form t)))
;;; END FIXME move into generalized Slynk infrastructure, or add to contrib mechanism

;;; Although by convention toString() is supposed to be a
;;; non-computationally expensive operation this isn't always the
;;; case, so make its computation a user interaction.
(defparameter *to-string-hashtable* (make-hash-table :weakness :key))

(defmethod emacs-inspect ((o t))
  (let* ((type (type-of o))
         (class (ignore-errors (find-class type)))
         (jclass (and (typep  class 'sys::built-in-class)
                      (jcall "getClass" o))))
    (let ((parts (sys:inspected-parts o)))
      `((:label "Type: ") (:value ,(or class type)) (:Newline)
        ,@(if jclass
              `((:label "Java type: ") (:value ,jclass) (:newline)))
        ,@(if parts
              (loop :for (label . value) :in parts
                 :appending (list
                             (list :label (string-capitalize label))
                             ": "
                             (list :value value (princ-to-string value)) '(:newline)))
              (list '(:label "No inspectable parts, dumping output of CL:DESCRIBE:")
                    '(:newline)
                    (with-output-to-string (desc) (describe o desc))))))))

(defmethod emacs-inspect ((string string))
  (slynk::lcons*
   '(:label "Value: ")  `(:value ,string ,(concatenate 'string "\"" string "\""))  '(:newline)
   #+abcl-introspect ;; ??? This doesn't appear depend on ABCL-INTROSPECT.  Why disable?
   `(:action "[Edit in emacs buffer]" ,(lambda() (slynk::ed-in-emacs `(:string ,string))))
   '(:newline)
   (if (ignore-errors (jclass string))
       `(:line "Names java class" ,(jclass string))
       "")
   #+abcl-introspect
   (if (and (jss-p)
            (stringp (funcall (intern "LOOKUP-CLASS-NAME" :jss) string :return-ambiguous t :muffle-warning t)))
       `(:multiple
         (:label "Abbreviates java class: ")
         ,(let ((it (funcall (intern "LOOKUP-CLASS-NAME" :jss) string :return-ambiguous t :muffle-warning t)))
           `(:value ,(jclass it)))
         (:newline))
       "")
   (if (ignore-errors (find-package (string-upcase string)))
       `(:line "Names package" ,(find-package (string-upcase string)))
       "")
   (let ((symbols (loop for p in (list-all-packages)
                        for found = (find-symbol (string-upcase string))
                        when (and found (eq (symbol-package found) p)
                                  (or (fboundp found)
                                      (boundp found)
                                      (symbol-plist found)
                                      (ignore-errors (find-class found))))
                          collect found)))
     (if symbols
         `(:multiple (:label "Names symbols: ")
                     ,@(loop for s in symbols
                             collect
                             (Let ((*package* (find-package :keyword)))
                               `(:value ,s ,(prin1-to-string s))) collect " ") (:newline))
         ""))
   (call-next-method)))

#+#.(slynk-backend:with-symbol 'java-exception 'java)
(defmethod emacs-inspect ((o java:java-exception))
  (append (call-next-method)
          (list '(:newline) '(:label "Stack trace")
                      '(:newline)
                      (let ((w (jnew "java.io.StringWriter")))
                        (jcall "printStackTrace" (java:java-exception-cause o) (jnew "java.io.PrintWriter" w))
                        (jcall "toString" w)))))

(defmethod emacs-inspect ((slot mop::slot-definition))
  `("Name: "
    (:value ,(mop:slot-definition-name slot))
    (:newline)
    "Documentation:" (:newline)
    ,@(when (slot-definition-documentation slot)
            `((:value ,(slot-definition-documentation slot)) (:newline)))
    "Initialization:" (:newline)
    (:label "  Args: ") (:value ,(mop:slot-definition-initargs slot)) (:newline)
    (:label "  Form: ")  ,(if (mop:slot-definition-initfunction slot)
                     `(:value ,(mop:slot-definition-initform slot))
                     "#<unspecified>") (:newline)
                     (:label "  Function: ")
                     (:value ,(mop:slot-definition-initfunction slot))
                     (:newline)))

(defmethod emacs-inspect ((f function))
  `(,@(when (function-name f)
        `((:label "Name: ")
          ,(princ-to-string (sys::any-function-name f)) (:newline)))
      ,@(multiple-value-bind (args present) (sys::arglist f)
          (when present
            `((:label "Argument list: ")
              ,(princ-to-string args)
              (:newline))))
      #+abcl-introspect
      ,@(when (documentation f t)
          `("Documentation:" (:newline)
                             ,(documentation f t) (:newline)))
      ,@(when (function-lambda-expression f)
          `((:label "Lambda expression:")
            (:newline) ,(princ-to-string
                         (function-lambda-expression f)) (:newline)))
      (:label "Function java class: ") (:value ,(jcall "getClass" f)) (:newline)
      #+abcl-introspect
      ,@(when (jcall "isInstance"  (java::jclass "org.armedbear.lisp.CompiledClosure") f)
          `((:label "Closed over: ")
            ,@(loop
                 for el in (sys::compiled-closure-context f)
                 collect `(:value ,el)
                 collect " ")
            (:newline)))
      #+abcl-introspect
      ,@(when (sys::get-loaded-from f)
          (list `(:label "Defined in: ")
                `(:value ,(sys::get-loaded-from f) ,(namestring (sys::get-loaded-from f)))
                '(:newline)))
      ;; I think this should work in older lisps too -- alanr
      ,@(let ((fields (jcall "getDeclaredFields" (jcall "getClass" f))))
          (when (plusp (length fields))
            (list* '(:label "Internal fields: ") '(:newline)
                   (loop for field across fields
                      do (jcall "setAccessible" field t) ;;; not a great idea esp. wrt. Java9
                      append
                        (let ((value (jcall "get" field f)))
                          (list "  "
                                `(:label ,(jcall "getName" field))
                                ": "
                                `(:value ,value ,(princ-to-string value))
                                '(:newline)))))))
      #+abcl-introspect
      ,@(when (and (function-name f) (symbolp (function-name f))
                   (eq (symbol-package (function-name f)) (find-package :cl)))
          (list '(:newline) (list :action "Lookup in hyperspec"
                                  (lambda () (hyperspec-do (symbol-name (function-name f))))
                                  :refreshp nil)
                '(:newline)))))

(defmethod emacs-inspect ((o java:java-object))
  (if (jinstance-of-p o (jclass "java.lang.Class"))
      (emacs-inspect-java-class o)
      (emacs-inspect-java-object o)))

(defvar *sly-tostring-on-demand* nil
  "Set to t if you don't want to automatically show toString() for java objects and instead have inspector action to compute")

(defun static-field? (field)
  ;; (plusp (logand #"reflect.Modifier.STATIC" (jcall "getModifiers" field)))
  ;; ugly replace with answer to avoid using jss
  (plusp (logand 8 (jcall "getModifiers" field))))

(defun inspector-java-object-fields (object)
  (loop
     for super = (java::jobject-class object) then (jclass-superclass super)
     while super
        ;;; NOTE: In the next line, if I write #'(lambda.... then I
        ;;; get an error compiling "Attempt to throw to the
        ;;; nonexistent tag DUPLICATABLE-CODE-P.". WTF
     for fields
       = (sort (jcall "getDeclaredFields" super) 'string-lessp :key (lambda(x) (jcall "getName" x)))
     for fromline
       = nil then (list `(:label "From: ") `(:value ,super  ,(jcall "getName" super)) '(:newline))
     when (and (plusp (length fields)) fromline)
     append fromline
     append
       (loop for this across fields
          for value = (jcall "get" (progn (jcall "setAccessible" this t) this) object)
          for line = `("  " (:label ,(jcall "getName" this)) ": " (:value ,value) (:newline))
          if (static-field? this)
          append line into statics
          else append line into members
          finally (return (append
                           (if members `((:label "Member fields: ") (:newline) ,@members))
                           (if statics `((:label "Static fields: ") (:newline) ,@statics)))))))

(defun emacs-inspect-java-object (object)
  (let ((to-string (lambda ()
                     (handler-case
                         (setf (gethash object *to-string-hashtable*)
                               (jcall "toString" object))
                       (t (e)
                         (setf (gethash object *to-string-hashtable*)
                               (format nil
                                       "Could not invoke toString(): ~A"
                                       e))))))
        (intended-class (cdr (assoc "intendedClass" (sys::inspected-parts object)
                                    :test 'equal))))
    `((:label "Class: ")
      (:value ,(jcall "getClass" object) ,(jcall "getName" (jcall "getClass" object) )) (:newline)
      ,@(if (and intended-class (not (equal intended-class (jcall "getName" (jcall "getClass" object)))))
            `((:label "Intended Class: ")
              (:value ,(jclass intended-class) ,intended-class) (:newline)))
      ,@(if (or (gethash object *to-string-hashtable*) (not *sly-tostring-on-demand*))
            (label-value-line "toString()" (funcall to-string))
            `((:action "[compute toString()]" ,to-string) (:newline)))
      ,@(inspector-java-object-fields object))))

(defmethod emacs-inspect ((slot mop::slot-definition))
  `("Name: "
    (:value ,(mop:slot-definition-name slot))
    (:newline)
    "Documentation:" (:newline)
    ,@(when (slot-definition-documentation slot)
            `((:value ,(slot-definition-documentation slot)) (:newline)))
    (:label "Initialization:") (:newline)
    (:label "  Args: ") (:value ,(mop:slot-definition-initargs slot)) (:newline)
    (:label "  Form: ")
    ,(if (mop:slot-definition-initfunction slot)
                     `(:value ,(mop:slot-definition-initform slot))
                     "#<unspecified>") (:newline)
                     "  Function: "
                     (:value ,(mop:slot-definition-initfunction slot))
                     (:newline)))

(defun inspector-java-fields (class)
  (loop
     for super
       = class then (jclass-superclass super)
     while super
     for fields
       = (jcall "getDeclaredFields" super)
     for fromline
       = nil then (list `(:label "From: ") `(:value ,super  ,(jcall "getName" super)) '(:newline))
     when (and (plusp (length fields)) fromline)
     append fromline
     append
       (loop for this across fields
          for pre = (subseq (jcall "toString" this)
                            0
                            (1+ (position #\. (jcall "toString" this)  :from-end t)))
          collect "  "
          collect (list :value this pre)
          collect (list :value this (jcall "getName" this) )
          collect '(:newline))))

(defun inspector-java-methods (class)
  (loop
     for super
       = class then (jclass-superclass super)
     while super
     for methods
       = (jcall "getDeclaredMethods" super)
     for fromline
       = nil then (list `(:label "From: ") `(:value ,super  ,(jcall "getName" super)) '(:newline))
     when (and (plusp (length methods)) fromline)
     append fromline
     append
       (loop for this across methods
          for desc = (jcall "toString" this)
          for paren =  (position #\( desc)
          for dot = (position #\. (subseq desc 0 paren) :from-end t)
          for pre = (subseq desc 0 dot)
          for name = (subseq desc dot paren)
          for after = (subseq desc paren)
          collect "  "
          collect (list :value this pre)
          collect (list :value this name)
          collect (list :value this after)
          collect '(:newline))))

(defun emacs-inspect-java-class (class)
  (let ((has-superclasses (jclass-superclass class))
        (has-interfaces (plusp (length (jclass-interfaces class))))
        (fields (inspector-java-fields class))
        (path (jcall "replaceFirst"
                     (jcall "replaceFirst"
                            (jcall "toString" (jcall "getResource"
                                                     class
                                                     (concatenate 'string
                                                                  "/" (substitute #\/ #\. (jcall "getName" class))
                                                                  ".class")))
                            "jar:file:" "") "!.*" "")))
    `((:label ,(format nil "Java Class: ~a" (jcall "getName" class) ))
      (:newline)
      ,@(when path (list `(:label ,"Loaded from: ")
                         `(:value ,path)
                         " "
                         `(:action "[open in emacs buffer]" ,(lambda() (slynk::ed-in-emacs `( ,path)))) '(:newline)))
      ,@(if has-superclasses
            (list* '(:label "Superclasses: ") (butlast (loop for super = (jclass-superclass class) then (jclass-superclass super)
                            while super collect (list :value super (jcall "getName" super)) collect ", "))))
      ,@(if has-interfaces
            (list* '(:newline) '(:label "Implements Interfaces: ")
                   (butlast (loop for i across (jclass-interfaces class) collect (list :value i (jcall "getName" i)) collect ", "))))
      (:newline) (:label "Methods:") (:newline)
      ,@(inspector-java-methods class)
      ,@(if fields
            (list*
             '(:newline) '(:label "Fields:") '(:newline)
             fields)))))

(defmethod emacs-inspect ((object sys::structure-object))
  `((:label "Type: ") (:value ,(type-of object)) (:newline)
    (:label "Class: ") (:value ,(class-of object)) (:newline)
    ,@(inspector-structure-slot-names-and-values object)))

(defun inspector-structure-slot-names-and-values (structure)
  (let ((structure-def (get (type-of structure) 'system::structure-definition)))
    (if structure-def
        `((:label "Slots: ") (:newline)
          ,@(loop for slotdef in (sys::dd-slots structure-def)
                  for name = (sys::dsd-name slotdef)
                  for reader = (sys::dsd-reader slotdef)
                  for value = (eval `(,reader ,structure))
                  append
                  `("  " (:label ,(string-downcase (string name))) ": " (:value ,value) (:newline))))
        `("No slots available for inspection."))))

(defmethod emacs-inspect ((object sys::structure-class))
  (let* ((name (jss::get-java-field object "name" t))
         (def (get name  'system::structure-definition)))
  `((:label "Class: ") (:value ,object) (:newline)
    (:label "Raw defstruct definition: ") (:value ,def  ,(let ((*print-array* nil)) (prin1-to-string def))) (:newline)
   ,@(parts-for-structure-def  name)
    ;; copy-paste from slynk fancy inspector
    ,@(when (slynk-mop:specializer-direct-methods object)
        `((:label "It is used as a direct specializer in the following methods:")
          (:newline)
          ,@(loop
              for method in (specializer-direct-methods object)
              for method-spec = (slynk::method-for-inspect-value method)
              collect "  "
              collect `(:value ,method ,(string-downcase (string (car method-spec))))
              collect `(:value ,method ,(format nil " (~{~a~^ ~})" (cdr method-spec)))
              append (let ((method method))
                       `(" " (:action "[remove]"
                                      ,(lambda () (remove-method (slynk-mop::method-generic-function method) method)))))
              collect '(:newline)
              if (documentation method t)
                collect "    Documentation: " and
              collect (slynk::abbrev-doc  (documentation method t)) and
              collect '(:newline)))))))

(defun parts-for-structure-def-slot (def)
  `((:label ,(string-downcase (sys::dsd-name def))) " reader: " (:value ,(sys::dsd-reader def) ,(string-downcase (string (sys::dsdreader def))))
    ", index: " (:value ,(sys::dsd-index def))
    ,@(if (sys::dsd-initform def)
          `(", initform: " (:value ,(sys::dsd-initform def))))
    ,@(if (sys::dsd-read-only def)
         '(", Read only"))))

(defun parts-for-structure-def (name)
  (let ((structure-def (get name 'system::structure-definition )))
    (append
     (loop for accessor in '(dd-name dd-conc-name dd-default-constructor dd-constructors dd-copier dd-include dd-type
                             dd-named dd-initial-offset dd-predicate dd-print-function dd-print-object
                             dd-inherited-accessors)
           for key = (intern (subseq (string accessor) 3) 'keyword)
           for fsym = (find-symbol (string accessor) 'system)
           for value = (eval `(,fsym ,structure-def))
           append `((:label ,(string-capitalize (string key))) ": " (:value ,value) (:newline)))
     (let* ((direct (sys::dd-direct-slots structure-def) )
           (all (sys::dd-slots structure-def))
           (inherited (set-difference all direct)))
     `((:label "Direct slots: ") (:newline)
       ,@(loop for slotdef in direct
               append `("  " ,@(parts-for-structure-def-slot slotdef)
                             (:newline)))
       ,@(if inherited
             (append '((:label "Inherited slots: ") (:newline))
                     (loop for slotdef in inherited
                           append `("  " (:label ,(string-downcase (string (sys::dsd-name slotdef))))
                                         (:value ,slotdef "slot definition")
                                         (:newline))))))))))



;; FIXME: I could't run this code
;;; TODO: move such invocations out of toplevel?
#+nil(eval-when (:load-toplevel)
  (unless (get 'sys::%print-unreadable-object 'sly-backend::sly-wrap)
    (wrap 'sys::%print-unreadable-object :more-informative :replace '%print-unreadable-object-java-too)))
