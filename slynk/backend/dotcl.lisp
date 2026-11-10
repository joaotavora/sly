;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; slynk-dotcl.lisp --- SLY backend for dotcl
;;;
;;; This code has been placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; dotcl is a Common Lisp implementation on .NET/CLR.  The facilities this
;;; backend needs are contrib modules shipped inside a dotcl release, pulled
;;; in by the system definition rather than from here:
;;;
;;;   dotcl-socket   sockets                   dotcl-thread   threads and locks
;;;   dotcl-gray     Gray streams
;;;
;;; The Gray streams package must exist before slynk-gray.lisp is compiled, which
;;; is why this file is loaded ahead of it.

(defpackage slynk/dotcl
  (:use cl slynk-backend))

(in-package slynk/dotcl)

;; The loader compiles this file directly, so the contrib modules are pulled
;; in here rather than through a system definition.
(require "dotcl-socket")
(require "dotcl-thread")
(require "dotcl-gray")

;;;; Connection info

(defimplementation gray-package-name ()
  "DOTCL-GRAY")

(defimplementation lisp-implementation-type-name ()
  "dotcl")

(defimplementation lisp-implementation-program ()
  "dotcl")

(defimplementation getpid ()
  (dotcl:getpid))

(defimplementation command-line-args ()
  nil)

(defimplementation quit-lisp ()
  #+ignore (ext:quit)) ; TODO

;;;; Communication style

(defimplementation preferred-communication-style ()
  :spawn)

;;;; UTF-8 encoding

(defimplementation string-to-utf8 (string)
  (dotcl:string-to-octets string :external-format :utf-8))

(defimplementation utf8-to-string (octets)
  (dotcl:octets-to-string octets :external-format :utf-8))

;;;; TCP Server (using dotcl-socket)

(defimplementation create-socket (host port &key backlog)
  (dotcl-socket:make-server-socket host port :backlog (or backlog 5)))

(defimplementation local-port (socket)
  (dotcl-socket:local-port socket))

(defimplementation close-socket (socket)
  (dotcl-socket:socket-close socket))

(defimplementation accept-connection (socket &key
                                      external-format
                                      buffering timeout)
  (declare (ignore external-format buffering timeout))
  (dotcl-socket:socket-accept socket))

;;;; Threading (using dotcl-thread)

(defvar *thread-id-counter* 0)
(defvar *thread-id-lock* (dotcl-thread:make-lock "thread-id-lock"))
(defvar *thread-id-map* (make-hash-table :test 'eq))
(defvar *thread-id-reverse* (make-hash-table :test 'eql))

(defun next-thread-id ()
  (dotcl-thread:with-lock-held (*thread-id-lock*)
    (incf *thread-id-counter*)))

(defun ensure-thread-id (thread)
  (or (gethash thread *thread-id-map*)
      (dotcl-thread:with-lock-held (*thread-id-lock*)
        (or (gethash thread *thread-id-map*)
            (let ((id (incf *thread-id-counter*)))
              (setf (gethash thread *thread-id-map*) id)
              (setf (gethash id *thread-id-reverse*) thread)
              id)))))

(defimplementation spawn (fn &key name)
  (let ((thread (dotcl-thread:make-thread fn :name (or name "anonymous"))))
    (ensure-thread-id thread)
    thread))

(defimplementation thread-id (thread)
  (ensure-thread-id thread))

(defimplementation find-thread (id)
  (gethash id *thread-id-reverse*))

(defimplementation thread-name (thread)
  (princ-to-string (dotcl-thread:thread-name thread)))

(defimplementation thread-status (thread)
  (if (dotcl-thread:thread-alive-p thread)
      "Running"
      "Stopped"))

(defimplementation current-thread ()
  (dotcl-thread:current-thread))

(defimplementation all-threads ()
  ;; Return known threads (no .NET API to list all managed threads)
  (let ((threads nil))
    (maphash (lambda (thread id)
               (declare (ignore id))
               (when (dotcl-thread:thread-alive-p thread)
                 (push thread threads)))
             *thread-id-map*)
    threads))

(defimplementation thread-alive-p (thread)
  (dotcl-thread:thread-alive-p thread))

(defimplementation interrupt-thread (thread fn)
  (declare (ignore thread fn))
  ;; TODO: .NET Thread.Interrupt or similar
  nil)

(defimplementation kill-thread (thread)
  (dotcl-thread:destroy-thread thread))

;;;; Mailbox (per-thread message queue)

(defvar *mailbox-lock* (dotcl-thread:make-lock "mailbox-lock"))
(defvar *mailboxes* (make-hash-table :test 'eq))

(defstruct (mailbox (:conc-name mailbox.))
  thread
  (lock (dotcl-thread:make-lock "mbox"))
  (queue nil :type list))

(defun mailbox (thread)
  "Return THREAD's mailbox, creating one if needed."
  (dotcl-thread:with-lock-held (*mailbox-lock*)
    (or (gethash thread *mailboxes*)
        (setf (gethash thread *mailboxes*)
              (make-mailbox :thread thread)))))

(defimplementation send (thread message)
  (let* ((mbox (mailbox thread))
         (lock (mailbox.lock mbox)))
    (dotcl-thread:with-lock-held (lock)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message))))))

(defimplementation receive-if (test &optional timeout)
  (let* ((mbox (mailbox (current-thread)))
         (lock (mailbox.lock mbox)))
    (assert (or (not timeout) (eq timeout t)))
    (loop
      (check-sly-interrupts)
      (dotcl-thread:with-lock-held (lock)
        (let* ((q (mailbox.queue mbox))
               (tail (member-if test q)))
          (when tail
            (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
            (return (car tail))))
        (when (eq timeout t) (return (values nil t))))
      ;; No condition variable yet — busy-wait with sleep
      (sleep 0.02))))

(defimplementation wake-thread (thread)
  ;; No condition variable — receive-if polls
  (declare (ignore thread))
  nil)

;;;; Locks

(defimplementation make-lock (&key name)
  (dotcl-thread:make-lock (or name "anonymous")))

(defimplementation call-with-lock-held (lock function)
  (dotcl-thread:with-lock-held (lock)
    (funcall function)))

;;;; Compilation (stubs)

(defimplementation call-with-compilation-hooks (func)
  (funcall func))

(defimplementation slynk-compile-string (string &key buffer position filename
                                         line column policy)
  (declare (ignore buffer position filename line column policy))
  (handler-case
      (progn (eval (read-from-string string))
             t)
    (error (c)
      (list :message (princ-to-string c)))))

(defimplementation slynk-compile-file (input-file output-file load-p
                                       external-format &key policy)
  (declare (ignore output-file external-format policy))
  (when load-p
    (load input-file)))

(defimplementation find-external-format (coding-system)
  (declare (ignore coding-system))
  :default)

(defimplementation guess-external-format (pathname)
  (declare (ignore pathname))
  :default)

;;;; Introspection (minimal)

(defimplementation arglist (name)
  (multiple-value-bind (arglist foundp)
      (dotcl:function-lambda-list name)
    (if foundp arglist :not-available)))

(defimplementation function-name (function)
  (declare (ignore function))
  nil)

(defimplementation valid-function-name-p (form)
  (or (symbolp form)
      (and (consp form) (eq (car form) 'setf) (symbolp (cadr form)))))

(defimplementation type-specifier-p (symbol)
  (declare (ignore symbol))
  nil)

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result nil))
    (when (fboundp symbol)
      (push (cons :function (documentation symbol 'function)) result))
    (when (boundp symbol)
      (push (cons :variable (documentation symbol 'variable)) result))
    result))

(defimplementation describe-definition (name type)
  (declare (ignore type))
  (format nil "~S" (if (fboundp name) (symbol-function name) name)))

(defimplementation find-definitions (name)
  (declare (ignore name))
  nil)

(defimplementation find-source-location (object)
  (declare (ignore object))
  nil)

;;;; Debugging

;;; DOTCL:BACKTRACE-WITH-ARGS lists the Lisp call stack, innermost first,
;;; one (NAME ARG...) per frame; NAME is the function's name as a string,
;;; without its package.  Only named functions have a frame.  The stack is
;;; taken once on entry to the debugger, and the frames of the debugger
;;; itself are cut off the top.
;;;
;;; DOTCL:FRAME-LOCALS numbers frames from the innermost frame at the time
;;; of the call, so a frame is remembered by its depth (frames between it
;;; and the bottom of the stack, itself included) and converted back to an
;;; index when asked.

(defvar *sly-db-frames* #()
  "Frames being debugged, innermost first, as (NAME ARG...) lists.")

(defvar *sly-db-bottom-depth* 0
  "Depth of the innermost frame in *SLY-DB-FRAMES*.")

(defparameter *debugger-entry-frames*
  '("INVOKE-SLY-DEBUGGER" "SLYNK-DEBUGGER-HOOK")
  "Frames through which slynk enters the debugger.  The innermost of them
and everything inside it belong to the debugger and are not shown.  (An
outer one belongs to an outer debugger level, whose frames are shown.)")

(defparameter *debugger-glue-frames*
  '("CALL-WITH-DEBUGGER-HOOK" "INVOKE-DEBUGGER" "SLYNK-DEBUGGER-HOOK")
  "Frames just outside an entry frame that are still part of entering the
debugger.")

(defun debugger-frame-count (frames)
  "How many of FRAMES, innermost first, belong to the debugger."
  (let ((entry (position-if (lambda (f) (member (car f) *debugger-entry-frames*
                                                 :test #'string=))
                            frames)))
    (cond (entry
           (loop for i from (1+ entry)
                 for f in (nthcdr (1+ entry) frames)
                 while (member (car f) *debugger-glue-frames* :test #'string=)
                 finally (return i)))
          (t
           ;; Entered some other way: drop only this function's own frame.
           (let ((self (position "CALL-WITH-DEBUGGING-ENVIRONMENT" frames
                                 :key #'car :test #'string=)))
             (if self (1+ self) 0))))))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* ((frames (dotcl:backtrace-with-args))
         (skip (debugger-frame-count frames))
         (*sly-db-frames* (coerce (nthcdr skip frames) 'vector))
         (*sly-db-bottom-depth* (- (length frames) skip)))
    (funcall debugger-loop-fn)))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook))
    (funcall fun)))

(defimplementation install-debugger-globally (function)
  (setf *debugger-hook* function))

(defimplementation compute-backtrace (start end)
  (let ((n (length *sly-db-frames*)))
    (coerce (subseq *sly-db-frames* (min start n) (if end (min end n) n))
            'list)))

(defun nth-frame (index)
  (if (< -1 index (length *sly-db-frames*))
      (aref *sly-db-frames* index)
      (error "No frame ~D." index)))

(defimplementation print-frame (frame stream)
  ;; The name is a string, but it reads as the symbol it stands for, so it
  ;; is written as text, not printed.
  (write-char #\( stream)
  (write-string (car frame) stream)
  (dolist (arg (cdr frame))
    (write-char #\Space stream)
    (prin1 arg stream))
  (write-char #\) stream))

(defun frame-function-symbol (name)
  "The symbol naming the function a frame called NAME runs.  Frames do not
record the package, so the accessible symbol is preferred and otherwise a
fbound symbol of that name in any package is taken."
  (let ((here (find-symbol name *package*)))
    (if (and here (fboundp here))
        here
        (let ((found nil))
          (do-all-symbols (s)
            (when (and (string= (symbol-name s) name) (fboundp s))
              (when (or (null found)
                        (and (not (dotcl:function-source-location found))
                             (dotcl:function-source-location s)))
                (setq found s))))
          found))))

(defimplementation frame-package (frame-number)
  (let ((sym (frame-function-symbol (car (nth-frame frame-number)))))
    (and sym (symbol-package sym))))

(defimplementation frame-source-location (frame-number)
  (let* ((name (car (nth-frame frame-number)))
         (sym (frame-function-symbol name))
         (loc (and sym (dotcl:function-source-location sym))))
    (if loc
        (make-location (list :file (namestring (truename (getf loc :file))))
                       (list :line (getf loc :line)))
        (make-error-location "No source location for ~A." name))))

(defun local-name (name frame-number)
  (or (find-symbol name (or (ignore-errors (frame-package frame-number))
                            *package*))
      (make-symbol name)))

(defun frame-argument-names (frame)
  "Names for a frame's arguments: the required and optional parameters of
its function where the lambda list is known, ARGn for the rest."
  (let* ((sym (frame-function-symbol (car frame)))
         (ll (and sym (ignore-errors (dotcl:function-lambda-list sym))))
         (names '()))
    (loop for p in ll
          until (member p '(&rest &body &key &aux &allow-other-keys))
          unless (eq p '&optional)
            do (push (symbol-name (if (consp p) (car p) p)) names))
    (loop for i from 0 below (length (cdr frame))
          collect (or (nth i (reverse names)) (format nil "ARG~D" i)))))

(defun frame-variables (frame-number)
  "(NAME . VALUE) pairs for a frame: its recorded lexical variables when the
code was compiled with DOTCL:*EMIT-FRAME-LOCALS* true, else its arguments."
  (let* ((frame (nth-frame frame-number))
         ;; DOTCL:FRAME-LOCALS counts from the innermost frame of this call,
         ;; so the stack is measured here, not in a helper with a frame of
         ;; its own.
         (here (length (dotcl:backtrace)))
         (recorded (dotcl:frame-locals
                    (- here (- *sly-db-bottom-depth* frame-number)))))
    (or recorded
        (mapcar #'cons (frame-argument-names frame) (cdr frame)))))

(defimplementation frame-locals (frame-number)
  ;; ID tells apart variables of the same name; Emacs shows it after the
  ;; name when it is not 0.
  (loop for (name . value) in (frame-variables frame-number)
        for seen = (list name) then (cons name seen)
        collect (list :name (local-name name frame-number)
                      :id (1- (count name seen :test #'string=))
                      :value value)))

(defimplementation frame-var-value (frame-number index)
  ;; INDEX is the variable's position in FRAME-LOCALS.
  (cdr (nth index (frame-variables frame-number))))

(defimplementation frame-catch-tags (frame-number)
  (declare (ignore frame-number))
  nil)

(defimplementation eval-in-frame (form frame-number)
  ;; The frame's variables are bound lexically around FORM.  A name FORM
  ;; mentions was interned when FORM was read, so FIND-SYMBOL finds it.
  (let ((bindings
          (loop for (name . value) in (remove-duplicates
                                       (frame-variables frame-number)
                                       :key #'car :test #'string=
                                       :from-end t)
                for sym = (find-symbol name *package*)
                when (and sym (not (constantp sym)))
                  collect (list sym (list 'quote value)))))
    (eval `(let ,bindings
             (declare (ignorable ,@(mapcar #'car bindings)))
             ,form))))

(defimplementation print-condition (condition stream)
  (format stream "~A" condition))

(defimplementation condition-extras (condition)
  (declare (ignore condition))
  nil)

;;;; Misc

(defimplementation call-with-syntax-hooks (fn)
  (funcall fn))

(defimplementation default-readtable-alist ()
  nil)

(defimplementation package-local-nicknames (package)
  (declare (ignore package))
  nil)

(defimplementation set-stream-timeout (stream timeout)
  (declare (ignore stream timeout))
  nil)

(defimplementation default-directory ()
  (namestring *default-pathname-defaults*))

(defimplementation set-default-directory (directory)
  (setf *default-pathname-defaults* (pathname directory))
  (default-directory))

(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  (macroexpand form))

;;; Register the current thread so SLY knows about it
(ensure-thread-id (dotcl-thread:current-thread))
