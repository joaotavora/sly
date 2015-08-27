(defpackage :slynk-stickers
  (:use :cl :slynk-api)
  (:import-from :slynk-backend :slynk-compile-string)
  (:import-from :slynk :defslyfun :compile-string-for-emacs)
  (:export #:record
           #:compile-for-stickers
           #:kill-stickers
           #:inspect-sticker
           #:inspect-sticker-recording
           #:fetch
           #:forget
           #:find-recording-or-lose
           #:search-for-recording
           #:toggle-break-on-stickers))
(in-package :slynk-stickers)

(defclass recording ()
  ((index :reader index-of)
   (ctime :initform (common-lisp:get-universal-time) :accessor ctime-of)
   (sticker :initform (error "required") :initarg :sticker :accessor sticker-of)
   (values :initform (error "required") :initarg :values :accessor values-of)
   (condition :initarg :condition :accessor condition-of)))

(defmethod initialize-instance :after ((x recording) &key sticker)
  (push x (recordings-of sticker))
  (setf (slot-value x 'index) (fill-pointer *recordings*))
  (vector-push-extend x *recordings*))

(defun recording-description-string (recording &optional stream print-first-value)
  (let ((values (values-of recording))
        (condition (condition-of recording)))
    (cond (condition
           (format stream "exited non-locally with: ~a" (slynk::to-line condition)))
          ((eq values 'exited-non-locally)
           (format stream "exited non-locally"))
          ((listp values)
           (if (and print-first-value
                    values)
               (format stream "~a" (slynk::to-line (car values)))
               (format stream "~a values" (length values))))
          (t
           (format stream "corrupt recording")))))

(defmethod print-object ((r recording) s)
  (print-unreadable-object (r s :type t)
    (recording-description-string r s)))

(defclass sticker ()
  ((id :initform (error "required")  :initarg :id :accessor id-of)
   (recordings :initform nil :accessor recordings-of)
   (ignore-spec :initform nil :accessor ignore-spec-of)))

(defmethod print-object ((sticker sticker) s)
  (print-unreadable-object (sticker s :type t)
    (format s "id: ~a" (id-of sticker))))

(defun exited-non-locally-p (recording)
  (when (or (condition-of recording)
            (eq (values-of recording) 'exited-non-locally))
    t))


;; FIXME: This won't work for multiple-connections. A channel, or some
;; connection specific structure, is needed for that.
;;
(defvar *stickers* (make-hash-table))
(defvar *recordings* (make-array 0 :fill-pointer 0 :adjustable t))
(defvar *visitor* nil)

(defslyfun compile-for-stickers (new-stickers
                                 dead-stickers
                                 instrumented-string
                                 original-string
                                 buffer
                                 position
                                 filename
                                 policy)
  "Considering NEW-STICKERS, compile INSTRUMENTED-STRING.
INSTRUMENTED-STRING is exerpted from BUFFER at POSITION. BUFFER may be
associated with FILENAME. DEAD-STICKERS if any, are killed. If
compilation succeeds, return a list (NOTES T).

If ORIGINAL-STRING, if non-nil, is compiled as a fallback if the
previous compilation. In this case a list (NOTES NIL) is returned or
an error is signalled.

If ORIGINAL-STRING is not supplied and compilation of
INSTRUMENTED-STRING fails, return NIL.

New stickers for NEW-STICKERS are registered in *STICKERS* and
stickers in DEAD-STICKERS are killed. NEW-STICKERS are not necessarily
\"new\" in the sense that the ids are not assigned by Slynk, but
their ignore-spec is reset nonetheless."
  ;; Dead stickers are unconditionally removed from *stickers*
  ;; 
  (kill-stickers dead-stickers)
  (let ((probe
          (handler-case
              (compile-string-for-emacs instrumented-string
                                        buffer
                                        position
                                        filename
                                        policy)
            (error () nil))))
    (cond (;; a non-nil and successful compilation result
           (and probe
                (third probe))
           ;; new objects for NEW-STICKERS are created
           (loop for id in new-stickers
                 do (setf (gethash id *stickers*)
                          (make-instance 'sticker :id id)))
           (list probe t))
          (original-string
           (list (compile-string-for-emacs original-string buffer position filename policy)
                 nil)))))

(defslyfun kill-stickers (ids)
  (loop for id in ids
        do (remhash id *stickers*)))

(define-condition sticker-related-condition (condition)
  ((sticker :initarg :sticker :initform (error "~S is required" 'sticker)
            :accessor sticker-of)
   (debugger-extra-options :initarg :debugger-extra-options
                           :accessor debugger-extra-options-of)))

(define-condition just-before-sticker (sticker-related-condition)
  ()
  (:report (lambda (c stream)
             (with-slots (sticker) c
               (print-unreadable-object (c stream :type t)
                 (format stream "~a" (id-of sticker)))))))

(define-condition right-after-sticker (sticker-related-condition)
  ((recording :initarg :recording :accessor recording-of))
  (:report (lambda (c stream)
             (with-slots (sticker recording) c
               (print-unreadable-object (c stream :type t)
                 (format stream "~a (recorded ~a)"
                         (id-of sticker)
                         recording))))))

(defparameter *break-on-stickers* nil
  "If non nil, RECORD breaks before and after recording sticker")

(defslyfun toggle-break-on-stickers ()
  "Toggle the value of *BREAK-ON-STICKERS*"
  (setq *break-on-stickers* (not *break-on-stickers*)))

(defun invoke-debugger-for-sticker (sticker condition)
  (restart-case
      (let ((*debugger-extra-options* (append (debugger-extra-options-of condition)
                                              *debugger-extra-options*)))
        (invoke-debugger condition))
    (continue () :report "OK, continue")
    (ignore-this-sticker ()
      :report "Stop bothering me about this sticker"
      :test (lambda (c)
              (cond ((typep c 'sticker-related-condition)
                     (and (eq (sticker-of c) sticker)
                          *break-on-stickers*))
                    (t
                     nil)))
      (setf (ignore-spec-of sticker)
            (list :before :after)))))

(defun call-with-sticker-recording (id fn)
  (let* ((sticker (gethash id *stickers*))
         (mark (gensym))
         (retval mark)
         (last-condition)
         (recording))
    (handler-bind ((condition (lambda (condition)
                                (setq last-condition condition))))
      ;; Maybe break before
      ;; 
      (when (and sticker
                 *break-on-stickers*
                 (not (member :before (ignore-spec-of sticker))))
        (invoke-debugger-for-sticker
         sticker (make-condition 'just-before-sticker
                                 :sticker sticker
                                 :debugger-extra-options
                                 `((:slynk-before-sticker ,id)))))
      ;; Run actual code under the sticker
      ;; 
      (unwind-protect
           (values-list (setq retval (multiple-value-list (funcall fn))))
        (when sticker
          ;; Always make a recording...
          ;;
          (setq recording
                (make-instance 'recording
                               :sticker sticker
                               :values (if (eq mark retval)
                                           'exited-non-locally
                                           retval)
                               :condition (and (eq mark retval) last-condition)))
          ;; ...and then maybe break after.
          (when (and *break-on-stickers*
                     (not (member :after (ignore-spec-of sticker))))
            (invoke-debugger-for-sticker
             sticker
             (make-condition 'right-after-sticker
                             :sticker sticker
                             :recording recording
                             :debugger-extra-options
                             `((:slynk-after-sticker
                                ,(describe-sticker-for-emacs sticker recording)))))))))))

(defmacro record (id &rest body)
  `(call-with-sticker-recording ,id (lambda () ,@body)))

(define-setf-expander record (x &environment env)
  (declare (ignore x env))
  (error "Sorry, not allowing ~S for ~S" 'setf 'record))

(defun search-for-recording-1 (from ignore-dead-p ignore-sticker-ids inc)
  (loop for starting-position in `(,from ,(if (plusp inc)
                                              -1
                                              (length *recordings*)))
        for inc in `(,inc ,(if (plusp inc) 1 -1))
          thereis (loop for candidate-id = (incf starting-position inc)
                        while (< -1 candidate-id (length *recordings*))
                        for recording = (aref *recordings* candidate-id)
                        for sticker-id = (id-of (sticker-of recording))
                        unless (or (member sticker-id ignore-sticker-ids)
                                   (and
                                    ignore-dead-p
                                    (not (gethash sticker-id *stickers*))))
                          return recording)))

(defun describe-recording-for-emacs (recording)
  "Describe RECORDING as (ID VALUE-DESCRIPTIONS EXITED-NON-LOCALLY-P)
ID is a number. VALUE-DESCRIPTIONS is a list of
strings. EXITED-NON-LOCALLY-P is an integer."
  (list (index-of recording)
        (and (listp (values-of recording))
             (mapcar #'slynk::to-line (values-of recording)))
        (exited-non-locally-p recording)))

(defun describe-sticker-for-emacs (sticker &optional recording)
  "Describe STICKER and either its latest recording or RECORDING.
Returns a list (ID NRECORDINGS . RECORDING-DESCRIPTION).
RECORDING-DESCRIPTION is as given by DESCRIBE-RECORDING-FOR-EMACS."
  (let* ((recordings (recordings-of sticker))
         (recording (or recording
                        (first recordings))))
    (list* (id-of sticker)
           (length recordings)
           (and recording
                (describe-recording-for-emacs recording)))))

(defslyfun search-for-recording (key ignore-spec dead-stickers direction)
  "Visit the next recording for the visitor KEY.
IGNORE-SPEC is a list (EXCLUDE-DEAD MORE...): ignore stickers whose ID
is in MORE and ignore dead stickers if EXCLUDE-DEAD.

Kill any stickers in DEAD-STICKERS.

DIRECTION can be the keyword :NEXT, :PREV or an integer recording
index.  If a recording can be found return a list (TOTAL-RECORDINGS
. STICKER-DESCRIPTION).  STICKER-DESCRIPTION is as given by
DESCRIBE-STICKER-FOR-EMACS.

Otherwise returns a list (NIL ERROR-DESCRIPTION)"
  (kill-stickers dead-stickers)
  (unless (and *visitor*
               (eq key (car *visitor*)))
    (setf *visitor* (cons key -1)))
  (let ((recording (if (numberp direction)
                       (ignore-errors (aref *recordings* direction))
                       (search-for-recording-1 (cdr *visitor*)
                                               (car ignore-spec)
                                               (cdr ignore-spec)
                                               (if (eq :next direction) 1 -1)))))
    (cond (recording
           (setf (cdr *visitor*)  (index-of recording))
           (list* (length *recordings*)
                  (describe-sticker-for-emacs (sticker-of recording) recording)))
          (t
           (list nil "No such recording")))))

(defslyfun fetch (dead-stickers)
  "Describe each known sticker to Emacs.
As always, take the opportunity to kill DEAD-STICKERS"
  (kill-stickers dead-stickers)
  (loop for sticker being the hash-values of *stickers*
        collect (describe-sticker-for-emacs sticker)))

(defslyfun forget (dead-stickers)
  "Forget all sticker recordings."
  (kill-stickers dead-stickers)
  (maphash (lambda (id sticker)
             (declare (ignore id))
             (setf (recordings-of sticker) nil))
           *stickers*)
  (setf (fill-pointer *recordings*) 0))

(defslyfun find-recording-or-lose (recording-id vindex)
  (let ((recording (aref *recordings* recording-id)))
    (if vindex
        (elt (values-of recording) vindex)
        (values-list (values-of recording)))))

(defun find-sticker-or-lose (id)
  (let ((probe (gethash id *stickers* :unknown)))
    (if (eq probe :unknown)
        (error "Cannot find sticker ~a" id)
        probe)))

(defslyfun inspect-sticker (sticker-id)
  (let ((sticker (find-sticker-or-lose sticker-id)))
    (slynk::inspect-object sticker)))

(defslyfun inspect-sticker-recording (recording-id vindex)
  (let ((recording (find-recording-or-lose recording-id vindex)))
    (slynk::inspect-object recording)))

(provide 'slynk-stickers)
