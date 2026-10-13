;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; slynk-clamiga.lisp --- SLY backend for clamiga (CL-Amiga).
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; clamiga is a from-scratch Common Lisp implementation (a C bytecode
;;; interpreter with an m68k JIT) targeting AmigaOS and POSIX hosts.  Its
;;; SLY-facing facilities live in three packages:
;;;
;;;   MP   threads, locks, condition variables   (src/core/builtins_thread.c)
;;;   EXT  sockets + introspection primitives     (builtins_io.c, builtins.c)
;;;   GRAY Gray streams                           (lib/gray-streams.lisp)
;;;
;;; PREREQUISITE: clamiga's Gray streams (the GRAY package) must exist before
;;; slynk-gray.lisp is compiled — they are not part of clamiga's boot image.
;;; This backend is loaded before slynk-gray in slynk-loader's *slynk-files*,
;;; so we satisfy the prerequisite here, using clamiga's own module loader:
;;; (require "gray-streams") finds lib/gray-streams.lisp on the source root
;;; (host: CWD; AmigaOS: PROGDIR:lib/).  No path knowledge is needed in the
;;; portable loader, and the generic slynk-loader / start-slynk.lisp path then
;;; brings clamiga up exactly like any other implementation.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "GRAY")
    (require "gray-streams")))

(defpackage slynk-clamiga
  (:use cl slynk-backend))

(in-package slynk-clamiga)


;;;; Slynk-mop
;;;
;;; clamiga's MOP accessors (CLASS-SLOTS, GENERIC-FUNCTION-LAMBDA-LIST,
;;; METHOD-SPECIALIZERS, ...) are interned in the CL package (defined by
;;; clos.lisp as it loads there).  Import whatever exists; everything the
;;; implementation lacks (EQL-SPECIALIZER, the slot-definition protocol,
;;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES, ...) is left as a SLYNK-MOP
;;; placeholder so SLYNK still reads cleanly and only errors if that exact
;;; feature is exercised.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((missing '()))
    (do-external-symbols (s :slynk-mop)
      (unless (find-symbol (string s) :cl)
        (push (string s) missing)))
    (import-slynk-mop-symbols :cl missing)))

(defimplementation gray-package-name ()
  "GRAY")


;;;; UTF-8
;;;
;;; clamiga sockets are byte streams and SLYNK does its own UTF-8 coding;
;;; the portable SLYNK-BACKEND defaults for STRING-TO-UTF8 / UTF8-TO-STRING
;;; are fine, so they are deliberately not overridden here.


;;;; TCP server

(defun loopback-p (host)
  (member host '("127.0.0.1" "localhost" "::1") :test #'equal))

(defimplementation create-socket (host port &key backlog)
  ;; clamiga manages the listen backlog internally; BACKLOG is advisory only.
  (declare (ignore backlog))
  (ext:socket-listen port (loopback-p host)))

(defimplementation local-port (socket)
  ;; Works even when SOCKET was opened on port 0 (OS-assigned ephemeral port):
  ;; clamiga records the bound port on the listening stream at listen time.
  (ext:socket-local-port socket))

(defimplementation close-socket (socket)
  (close socket))

(defimplementation accept-connection (socket &key external-format
                                            buffering timeout)
  ;; The accepted stream is a bidirectional byte stream that also supports
  ;; READ-/WRITE-CHAR; SLYNK reads octets off it with READ-SEQUENCE and does
  ;; the UTF-8 itself, so EXTERNAL-FORMAT/BUFFERING/TIMEOUT do not apply.
  (declare (ignore external-format buffering timeout))
  (ext:socket-accept socket))

(defimplementation preferred-communication-style ()
  ;; clamiga always has the MP threading package.
  :spawn)

(defimplementation find-external-format (coding-system)
  ;; We never hand an external-format to the (byte) socket stream; just make
  ;; sure CREATE-SERVER does not reject the requested coding system.
  (if (member coding-system '("utf-8" "utf-8-unix" "utf-8-dos" "utf-8-mac")
              :test #'string=)
      :utf-8
      :default))


;;;; Unix integration

(defimplementation getpid ()
  ;; clamiga exposes no getpid(); SLYNK only uses this for the connection
  ;; banner, so a constant is harmless.
  0)

(defimplementation lisp-implementation-type-name ()
  "clamiga")

(defimplementation quit-lisp ()
  (let ((q (or (find-symbol "QUIT" :cl-user)
               (find-symbol "EXIT" :cl-user))))
    (if q
        (funcall q)
        (error "clamiga: no QUIT/EXIT function available."))))

(defimplementation command-line-args ()
  ;; clamiga does not expose argv to Lisp.
  '())


;;;; Pathnames / working directory

(defimplementation default-directory ()
  (namestring (ext:getcwd)))

(defimplementation set-default-directory (directory)
  ;; No chdir() binding; adjust *DEFAULT-PATHNAME-DEFAULTS* so relative
  ;; pathname resolution at least tracks the buffer's directory.
  (setf *default-pathname-defaults* (pathname directory))
  (default-directory))


;;;; Compilation
;;;
;;; clamiga compiles every form to bytecode on EVAL, so "compiling" a string
;;; is just reading and evaluating it.  There is no compiler-condition stream
;;; yet, so inline warning annotations are not produced; failures surface as
;;; ordinary conditions.

(defimplementation call-with-compilation-hooks (function)
  (funcall function))

(defimplementation slynk-compile-string (string &key buffer position
                                                filename line column policy)
  (declare (ignore buffer position filename line column policy))
  (with-compilation-hooks ()
    (handler-case
        (progn
          (with-input-from-string (s string)
            (loop for form = (read s nil s)
                  until (eq form s)
                  do (eval form)))
          t)
      (error () nil))))

(defimplementation slynk-compile-file (input-file output-file
                                       load-p external-format
                                       &key policy)
  (declare (ignore external-format policy))
  (with-compilation-hooks ()
    (multiple-value-bind (fasl warnings-p failure-p)
        (compile-file input-file :output-file output-file)
      (when (and load-p fasl (not failure-p))
        (load fasl))
      (values fasl warnings-p failure-p))))


;;;; Documentation / introspection

(defun designator-function (name)
  "Resolve a function designator NAME to a function object, or NIL."
  (cond ((functionp name) name)
        ((and (valid-function-name-p name) (fboundp name))
         (ignore-errors (fdefinition name)))
        (t nil)))

(defun generic-function-p (object)
  (and object
       (ignore-errors (typep object 'slynk-mop:standard-generic-function))))

(defimplementation arglist (name)
  (let ((fn (designator-function name)))
    (cond
      ;; clamiga's EXT:FUNCTION-ARGLIST covers ordinary functions, closures
      ;; and C builtins, but not generic functions; those carry their lambda
      ;; list on the GF struct.
      ((generic-function-p fn)
       (or (ignore-errors (slynk-mop:generic-function-lambda-list fn))
           :not-available))
      (t
       (let ((al (ignore-errors (ext:function-arglist (or fn name)))))
         (if (and al (not (eq al :not-available)))
             al
             :not-available))))))

(defimplementation function-name (function)
  (cond ((generic-function-p function)
         (ignore-errors (slynk-mop:generic-function-name function)))
        (t nil)))

(defimplementation type-specifier-p (symbol)
  (handler-case (progn (typep nil symbol) t)
    (error () nil)))

(defimplementation macroexpand-all (form &optional env)
  ;; clamiga has no code walker; expand the top-level form only.
  (macroexpand form env))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((frob (type boundp)
             (when (funcall boundp symbol)
               (setf result
                     (list* type (or (describe-definition symbol type)
                                     :not-documented)
                            result)))))
      (frob :variable #'boundp)
      (frob :function #'fboundp)
      (frob :class (lambda (s) (find-class s nil))))
    result))

(defimplementation describe-definition (name type)
  (case type
    (:variable (documentation name 'variable))
    (:function (documentation name 'function))
    (:class    (documentation name 'type))
    (t nil)))


;;;; Apropos
;;;
;;; A simple substring matcher.  (Note: SLYNK's APROPOS-LIST-FOR-EMACS also
;;; relies on CL:DO-ALL-SYMBOLS, which clamiga does not yet provide, so the
;;; apropos command may be unavailable until that lands.)

(defimplementation make-apropos-matcher (pattern symbol-name-fn
                                                 &optional case-sensitive)
  (let ((pattern (if case-sensitive pattern (string-upcase pattern))))
    (lambda (symbol)
      (let ((name (funcall symbol-name-fn symbol)))
        (let ((name (if case-sensitive name (string-upcase name))))
          (let ((pos (search pattern name)))
            (when pos
              (list pos (+ pos (length pattern))))))))))


;;;; Definition finding

(defun source-location-of (object)
  "Turn EXT:FUNCTION-SOURCE-LOCATION's (FILE LINE) into a SLY location."
  (let ((loc (ignore-errors (ext:function-source-location object))))
    (when (and (consp loc) (stringp (first loc)) (integerp (second loc)))
      (destructuring-bind (file line) loc
        (make-location `(:file ,file) `(:line ,line))))))

(defimplementation find-definitions (name)
  (let* ((fn (designator-function name))
         (loc (and fn (source-location-of fn))))
    (when loc
      (list (list `(defun ,name) loc)))))

(defimplementation find-source-location (object)
  (or (source-location-of object)
      (make-error-location "No source location for ~S." object)))


;;;; Debugging (SLDB)
;;;
;;; clamiga snapshots the error-time call frames when *DEBUGGER-HOOK* fires,
;;; so EXT:BACKTRACE must be read from within the debugging environment.  Each
;;; frame is (INDEX NAME FILE LINE), innermost first; the INDEX equals its
;;; position, so frame numbers map straight onto EXT:FRAME-LOCALS.

(defvar *clamiga-backtrace* '()
  "The frames captured by CALL-WITH-DEBUGGING-ENVIRONMENT.")

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function))

(defimplementation call-with-debugger-hook (hook function)
  (let ((*debugger-hook* hook))
    (funcall function)))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let ((*clamiga-backtrace* (or (ignore-errors (ext:backtrace)) '())))
    (funcall debugger-loop-fn)))

(defimplementation compute-backtrace (start end)
  (let ((bt *clamiga-backtrace*))
    (subseq bt start (and (numberp end) (min end (length bt))))))

(defimplementation print-frame (frame stream)
  (destructuring-bind (index name file line) frame
    (declare (ignore index))
    (format stream "~A~@[ (~A~@[:~A~])~]"
            (or name "#<anonymous>")
            (and (stringp file) file)
            line)))

(defimplementation frame-source-location (frame-number)
  (let ((frame (nth frame-number *clamiga-backtrace*)))
    (if frame
        (destructuring-bind (index name file line) frame
          (declare (ignore index name))
          (if (and (stringp file) (integerp line))
              (make-location `(:file ,file) `(:line ,line))
              (make-error-location "No source for frame ~A." frame-number)))
        (make-error-location "No such frame: ~A." frame-number))))

(defimplementation frame-locals (frame-number)
  (let ((locals (ignore-errors (ext:frame-locals frame-number))))
    (when (consp locals)
      (loop for (name . value) in locals
            collect (list :name name :id 0 :value value)))))

(defimplementation frame-var-value (frame-number var-number)
  (let ((locals (ext:frame-locals frame-number)))
    (cdr (nth var-number locals))))

(defimplementation frame-catch-tags (frame-number)
  (declare (ignore frame-number))
  '())

(defimplementation frame-restartable-p (frame)
  (declare (ignore frame))
  nil)


;;;; Multithreading

(defimplementation spawn (fn &key name)
  (mp:make-thread fn :name (or name "slynk worker")))

(defvar *thread-id-counter* 0)
(defvar *thread-id-map* (make-hash-table)
  "id -> thread.  Dead threads are pruned lazily on access.")
(defvar *thread-id-map-lock* (mp:make-lock "slynk thread id map"))

(defimplementation thread-id (thread)
  (call-with-lock-held
   *thread-id-map-lock*
   (lambda ()
     (let ((found nil) (dead '()))
       (maphash (lambda (id th)
                  (cond ((not (mp:thread-alive-p th)) (push id dead))
                        ((eq th thread) (setf found id))))
                *thread-id-map*)
       (dolist (id dead) (remhash id *thread-id-map*))
       (or found
           (let ((id (incf *thread-id-counter*)))
             (setf (gethash id *thread-id-map*) thread)
             id))))))

(defimplementation find-thread (id)
  (call-with-lock-held
   *thread-id-map-lock*
   (lambda ()
     (let ((th (gethash id *thread-id-map*)))
       (cond ((and th (mp:thread-alive-p th)) th)
             (t (when th (remhash id *thread-id-map*)) nil))))))

(defimplementation thread-name (thread)
  (mp:thread-name thread))

(defimplementation thread-status (thread)
  (if (mp:thread-alive-p thread) "Running" "Dead"))

(defimplementation current-thread ()
  (mp:current-thread))

(defimplementation all-threads ()
  (mp:all-threads))

(defimplementation thread-alive-p (thread)
  (mp:thread-alive-p thread))

(defimplementation interrupt-thread (thread fn)
  (mp:interrupt-thread thread fn))

(defimplementation kill-thread (thread)
  (mp:destroy-thread thread))

;;; Mailboxes — clamiga has no native message passing, so build one out of a
;;; lock + condition variable per thread (the ECL backend's approach).

(defvar *mailbox-lock* (mp:make-lock "slynk mailbox registry"))
(defvar *mailboxes* '()
  "List of all mailboxes.")

(defstruct (mailbox (:conc-name mailbox.))
  thread
  (lock (mp:make-lock "slynk mailbox"))
  (cvar (mp:make-condition-variable "slynk mailbox"))
  (queue '() :type list))

(defun mailbox (thread)
  "Return THREAD's mailbox, creating it on first use."
  (call-with-lock-held
   *mailbox-lock*
   (lambda ()
     (or (find thread *mailboxes* :key #'mailbox.thread)
         (let ((mb (make-mailbox :thread thread)))
           (push mb *mailboxes*)
           mb)))))

(defimplementation send (thread message)
  (let* ((mbox (mailbox thread))
         (lock (mailbox.lock mbox)))
    (call-with-lock-held
     lock
     (lambda ()
       (setf (mailbox.queue mbox)
             (nconc (mailbox.queue mbox) (list message)))
       (mp:condition-broadcast (mailbox.cvar mbox))))))

(defimplementation receive-if (test &optional timeout)
  (let* ((mbox (mailbox (current-thread)))
         (lock (mailbox.lock mbox)))
    (assert (or (not timeout) (eq timeout t)))
    (loop
      (check-sly-interrupts)
      ;; Decide what to do while holding the lock, but perform the RETURN
      ;; *outside* CALL-WITH-LOCK-HELD.  clamiga drops the secondary value of a
      ;; multiple-value RETURN-FROM that unwinds through an UNWIND-PROTECT (the
      ;; one in CALL-WITH-LOCK-HELD), so `(return-from receive-if (values nil t))`
      ;; from inside the lambda would lose the TIMED-OUT-P flag -- SLYNK's
      ;; PROCESS-REQUESTS then gets (NIL NIL) and dies with
      ;; "destructure-case failed: NIL".  Returning after the lock is released
      ;; keeps both values intact.
      (let ((message nil) (got nil) (timed-out nil))
        (call-with-lock-held
         lock
         (lambda ()
           (let* ((q (mailbox.queue mbox))
                  (tail (member-if test q)))
             (cond (tail
                    (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
                    (setf message (car tail) got t))
                   ((eq timeout t)
                    (setf timed-out t))
                   (t
                    ;; Wake periodically so CHECK-SLY-INTERRUPTS gets a turn.
                    (mp:condition-wait (mailbox.cvar mbox) lock 0.2))))))
        (cond (got (return message))
              (timed-out (return (values nil t))))))))

;;; Thread registry for SLYNK's named service threads.

(let ((alist '())
      (lock (mp:make-lock "slynk register-thread")))
  (defimplementation register-thread (name thread)
    (declare (type symbol name))
    (call-with-lock-held
     lock
     (lambda ()
       (cond ((null thread)
              (setf alist (delete name alist :key #'car)))
             (t
              (let ((probe (assoc name alist)))
                (if probe
                    (setf (cdr probe) thread)
                    (setf alist (acons name thread alist))))))))
    nil)
  (defimplementation find-registered (name)
    (call-with-lock-held lock (lambda () (cdr (assoc name alist))))))


;;;; Locks

(defimplementation make-lock (&key name)
  ;; Must be RECURSIVE.  SLY's Gray output stream re-enters its own buffer-write
  ;; lock: STREAM-WRITE-STRING holds it (WITH-SLY-OUTPUT-STREAM) and, when the
  ;; incoming chunk doesn't fit, calls STREAM-FINISH-OUTPUT, which re-acquires
  ;; the same lock (slynk-gray.lisp:77,84,116).  clamiga's MP:MAKE-LOCK is a
  ;; non-recursive pthread mutex, so that self-re-acquire deadlocks -- e.g.
  ;; bi_load's C-level "; Loading" write into *standard-output* during
  ;; (asdf:load-system ...) over the mREPL wedges the eval thread.  CCL's locks
  ;; are recursive by default and SLY's code is written against that; match it.
  ;; SLY uses locks purely as guards and never relies on non-recursive
  ;; deadlock detection, so this is safe.
  (mp:make-recursive-lock (or name "slynk lock")))

(defimplementation call-with-lock-held (lock function)
  (mp:acquire-lock lock)
  (unwind-protect (funcall function)
    (mp:release-lock lock)))


;;;; Input polling
;;;
;;; clamiga has no select()/poll() binding and CL:LISTEN does not report
;;; readability on socket streams, so this is best-effort.  With the :SPAWN
;;; communication style (the only one we offer) SLYNK drives all events
;;; through the mailbox and a dedicated blocking reader thread, so this is off
;;; the hot path; it exists mainly so the interface is bound.

(defimplementation wait-for-input (streams &optional timeout)
  (assert (member timeout '(nil t)))
  (loop
    (cond ((check-sly-interrupts) (return :interrupt))
          (t
           (let ((ready (remove-if-not #'listen streams)))
             (cond (ready (return ready))
                   (timeout (return '()))
                   (t (sleep 0.1))))))))
