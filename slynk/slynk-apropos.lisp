(defpackage :slynk-apropos
  (:use #:cl #:slynk-api)
  (:export
   #:apropos-list-for-emacs))

(in-package :slynk-apropos)

(defun excluded-from-searches-p (symbol)
  "Tell if SYMBOL should be excluded from \"apropos\" or completion."
  (some (lambda (fn) (funcall fn symbol)) *exclude-symbol-functions*))

(defslyfun apropos-list-for-emacs  (name &optional external-only
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
    (let ((*buffer-package* (or package
                                slynk::*slynk-io-package*)))
      (loop for (symbol . extra)
              in (sort (remove-duplicates
                        (apropos-symbols name external-only case-sensitive package)
                        :key #'first)
                       #'present-symbol-before-p
                       :key #'first)
            for short = (briefly-describe-symbol-for-emacs symbol)
            when short
              collect (append short extra)))))

(defun briefly-describe-symbol-for-emacs (symbol)
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
                               (symbol-external-p symbol))
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
  (let ((matcher (funcall (read-from-string "cl-ppcre:create-scanner")
                          pattern
                          :case-insensitive-mode (not case-sensitive))))
    (lambda (symbol)
      (funcall (read-from-string "cl-ppcre:scan")
               matcher
               (funcall symbol-name-fn symbol)))))

(defun make-plain-matcher (pattern case-sensitive symbol-name-fn)
  (let ((chr= (if case-sensitive #'char= #'char-equal)))
    (lambda (symbol)
      (let((beg (search pattern
                        (funcall symbol-name-fn symbol)
                        :test chr=)))
        (when beg
          (values beg (+ beg (length pattern))))))))

(defparameter *try-cl-ppcre-for-apropos* t
  "If non-NIL, maybe try CL-PPCRE for apropos requests.
CL-PPCRE must be loaded. This option has no effect if the
MAKE-APROPOS-MATCHER interface has been implemented.")

(defun apropos-symbols (pattern external-only case-sensitive package)
  "Search for symbols matching PATTERN."
  (let* ((packages (or package (remove (find-package :keyword)
                                       (list-all-packages))))
         (symbol-name-fn
           (lambda (symbol)
             (cond ((not package)
                    ;; include qualifier in search if user didn't pass
                    ;; PACKAGE.
                    (concatenate 'string
                                 (package-name (symbol-package symbol))
                                 (if (symbol-external-p symbol) ":" "::")
                                 (symbol-name symbol)))
                   (t
                    (string symbol)))))
         (interface-unimplemented-p
           (find 'slynk-backend:make-apropos-matcher
                 slynk-backend::*unimplemented-interfaces*))
         (attempt-cl-ppcre (and *try-cl-ppcre-for-apropos*
                                (not (every #'alpha-char-p pattern))))
         (cl-ppcre-matcher (and attempt-cl-ppcre
                                (find-package :cl-ppcre)
                                (ignore-errors
                                 (make-cl-ppcre-matcher pattern case-sensitive symbol-name-fn))))
         (matcher (cond ((and interface-unimplemented-p
                              attempt-cl-ppcre
                              cl-ppcre-matcher)
                         ;; Use regexp apropos we guess the user has
                         ;; requested it and if it is possible.
                         ;;
                         (background-message "Using CL-PPCRE for apropos on regexp \"~a\"" pattern)
                         cl-ppcre-matcher)
                        (interface-unimplemented-p
                         ;; Use plain apropos otherwise
                         ;; 
                         (when attempt-cl-ppcre
                           (if (not (find-package :cl-ppcre))
                               (background-message "Using plain apropos. Load CL-PPCRE to enable regexps")
                               (background-message "Not a valid CL-PPCRE regexp, so using plain apropos")))
                         (make-plain-matcher pattern case-sensitive symbol-name-fn))
                        (t
                         (slynk-backend:make-apropos-matcher pattern
                                                             symbol-name-fn
                                                             case-sensitive)))))
    (with-package-iterator (next packages :external :internal)
      (loop for (morep symbol) = (multiple-value-list (next))
            while morep
            for (match end) = (and (not (excluded-from-searches-p symbol))
                                   (or (not external-only)
                                       (symbol-external-p symbol))
                                   (symbol-package symbol)
                                   (multiple-value-list (funcall matcher symbol)))
            when match
              collect `(,symbol ,@(when end `(:bounds ((,match ,end)))))))))
