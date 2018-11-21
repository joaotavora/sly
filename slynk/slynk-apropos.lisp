(defpackage :slynk-apropos
  (:use #:cl #:slynk-api)
  (:export
   #:apropos-list-for-emacs
   #:*preferred-apropos-matcher*))

(in-package :slynk-apropos)

(defparameter *preferred-apropos-matcher* 'make-flex-matcher
  "Preferred matcher for apropos searches.
Value is a function of three arguments , PATTERN, CASE-SENSITIVE and
SYMBOL-NAME-FN that should return a function, called MATCHER of one
argument, a SYMBOL.  MATCHER should return non-nil if PATTERN somehow
matches the result of applying SYMBOL-NAME-FN to SYMBOL, according to
CASE-SENSITIVE.  The non-nil return value can be a list of integer or
a list of lists of integers.")

(defslyfun apropos-list-for-emacs  (pattern &optional external-only
                                            case-sensitive package)
  "Make an apropos search for Emacs.
The result is a list of property lists."
  (let ((package (if package
                     (or (parse-package package)
                         (error "No such package: ~S" package)))))
    ;; The MAPCAN will filter all uninteresting symbols, i.e. those
    ;; who cannot be meaningfully described.
    ;;
    ;; *BUFFER-PACKAGE* is exceptionally set so that the symbol
    ;; listing will only omit package qualifier iff the user specified
    ;; PACKAGE.
    (let* ((*buffer-package* (or package
                                 slynk::*slynk-io-package*))
           (symbol-name-fn
             (if package
                 (lambda (symbol) (string symbol))
                 (lambda (symbol)
                   (concatenate 'string
                                (package-name (symbol-package symbol))
                                ":"
                                (symbol-name symbol)))))
           (matcher (funcall *preferred-apropos-matcher*
                             pattern
                             case-sensitive
                             symbol-name-fn))
           (seen (make-hash-table))
           result)

      (do-all-symbols (sym)
        (multiple-value-bind (bounds score)
            (funcall matcher sym)
          (unless (gethash sym seen)
            (when bounds
              (let ((external (symbol-external-p sym)))
                (unless (or (and external-only
                                 (not external))
                            (and package
                                 (not (eq package (symbol-package sym)))))
                  (push `(,sym :bounds ,bounds
                               ,@(and score `(:flex-score ,score))
                               :external-p ,external)
                        result))))
            (setf (gethash sym seen) t))))
      (loop for (symbol . extra)
              in (sort result
                       (lambda (x y)
                         (let ((scorex (getf (cdr x) :flex-score))
                               (scorey (getf (cdr y) :flex-score)))
                           (if (and scorex scorey)
                               (> scorex scorey)
                               (present-symbol-before-p (car x) (car y))))))
            for short = (briefly-describe-symbol-for-emacs
                         symbol (getf extra :external-p))
            for score = (getf extra :flex-score)
            when score
              do (setf (getf extra :flex-score)
                       (format nil "~2$%"
                               (* 100 score)))
            do (remf extra :external-p)
            when short
              collect (append short extra)))))

(defun briefly-describe-symbol-for-emacs (symbol external-p)
  "Return a property list describing SYMBOL.
Like `describe-symbol-for-emacs' but with at most one line per item."
  (flet ((first-line (string)
           (let ((pos (position #\newline string)))
             (if (null pos) string (subseq string 0 pos)))))
    (let ((desc (map-if #'stringp #'first-line
                        (slynk-backend:describe-symbol-for-emacs symbol))))
      (if desc
          `(:designator ,(list (symbol-name symbol)
                               (let ((package (symbol-package symbol)))
                                 (and package
                                      (package-name package)))
                               external-p)
                        ,@desc
                        ,@(let ((arglist (and (fboundp symbol)
                                              (slynk-backend:arglist symbol))))
                            (when (and arglist
                                       (not (eq arglist :not-available)))
                              `(:arglist ,(princ-to-string arglist)))))))))

(defun present-symbol-before-p (x y)
  "Return true if X belongs before Y in a printed summary of symbols.
Sorted alphabetically by package name and then symbol name, except
that symbols accessible in the current package go first."
  (declare (type symbol x y))
  (flet ((accessible (s)
           ;; Test breaks on NIL for package that does not inherit it
           (eq (find-symbol (symbol-name s) *buffer-package*) s)))
    (let ((ax (accessible x)) (ay (accessible y)))
      (cond ((and ax ay) (string< (symbol-name x) (symbol-name y)))
            (ax t)
            (ay nil)
            (t (let ((px (symbol-package x)) (py (symbol-package y)))
                 (if (eq px py)
                     (string< (symbol-name x) (symbol-name y))
                     (string< (package-name px) (package-name py)))))))))

(defun make-cl-ppcre-matcher (pattern case-sensitive symbol-name-fn)
  (if (not (every #'alpha-char-p pattern))
      (cond ((find-package :cl-ppcre)
             (background-message "Using CL-PPCRE for apropos on regexp \"~a\"" pattern)

             (let ((matcher (funcall (read-from-string "cl-ppcre:create-scanner")
                                     pattern
                                     :case-insensitive-mode (not case-sensitive))))
               (lambda (symbol)
                 (multiple-value-bind (beg end)
                     (funcall (read-from-string "cl-ppcre:scan")
                              matcher
                              (funcall symbol-name-fn symbol))
                   (when beg `((,beg ,end)))))))
            (t
             (background-message "Using plain apropos. Load CL-PPCRE to enable regexps")
             (make-plain-matcher pattern case-sensitive symbol-name-fn)))
      (make-plain-matcher pattern case-sensitive symbol-name-fn)))

(defun make-plain-matcher (pattern case-sensitive symbol-name-fn)
  (let ((chr= (if case-sensitive #'char= #'char-equal)))
    (lambda (symbol)
      (let ((beg (search pattern
                         (funcall symbol-name-fn symbol)
                         :test chr=)))
        (when beg
          `((,beg ,(+ beg (length pattern)))))))))

(defun make-flex-matcher (pattern case-sensitive symbol-name-fn)
  (if (zerop (length pattern))
      (make-plain-matcher pattern case-sensitive symbol-name-fn)
      (let ((chr= (if case-sensitive #'char= #'char-equal)))
        (lambda (symbol)
          (slynk-completion:flex-matches
           pattern (funcall symbol-name-fn symbol) symbol chr=)))))

