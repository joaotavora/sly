;;; slynk-flex-completion.lisp --- Common Lisp symbol completion routines
;;
;; Authors: João Távora, some parts derivative works of SLIME, by its
;; authors.
;;
(defpackage :slynk-completion
  (:use #:cl #:slynk-api)
  (:import-from #:slynk-backend
                #:mappend
                #:when-let)
  (:export
   #:flex-completions
   #:simple-completions)
  (:documentation "This package provides two strategies for completion: Simple
  completion and flex/fuzzy completion.

Simple completion looks for the pattern in the beginning of the symbols.

Flex completion ranks the match with a combination of the candidate length and
the number of characters from the pattern found in the candidate as well as the
distance between the matching characters. Taking into account the length of the
candidate has the unfortunate consequence that candidates that don't contain
any character from the pattern are ranked by length alone, the shorter the
better.

See ido.el For a more thorough description."))

(in-package :slynk-completion)


;;; Simple completion
;;; 
(defslyfun simple-completions (prefix package)
  "Return a list of completions for the string PREFIX."
  (let ((strings (all-simple-completions prefix package)))
    (list strings (longest-common-prefix strings))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'simple-completions :slynk)
  (export 'simple-completions :slynk))

(defun all-simple-completions (prefix package)
  (multiple-value-bind (name pname intern) (tokenize-symbol prefix)
    (let* ((extern (and pname (not intern)))
	   (pkg (cond ((equal pname "") keyword-package)
                      ((not pname) (guess-buffer-package package))
                      (t (guess-package pname))))
	   (test (lambda (sym) (prefix-match-p name (symbol-name sym))))
	   (syms (and pkg (matching-symbols pkg extern test)))
           (strings (loop for sym in syms
                          for str = (unparse-symbol sym)
                          when (prefix-match-p name str) ; remove |Foo|
                          collect str)))
      (format-completion-set strings intern pname))))

(defun matching-symbols (package external test)
  (let ((test (if external
		  (lambda (s)
		    (and (symbol-external-p s package)
			 (funcall test s)))
		  test))
	(result '()))
    (do-symbols (s package)
      (when (funcall test s)
	(push s result)))
    (remove-duplicates result)))

(defun unparse-symbol (symbol)
  (let ((*print-case* (case (readtable-case *readtable*)
                        (:downcase :upcase)
                        (t :downcase))))
    (unparse-name (symbol-name symbol))))

(defun prefix-match-p (prefix string)
  "Return true if PREFIX is a prefix of STRING."
  (not (mismatch prefix string :end2 (min (length string) (length prefix))
                 :test #'char-equal)))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun format-completion-set (strings internal-p package-name)
  "Format a set of completion strings.
Returns a list of completions with package qualifiers if needed."
  (mapcar (lambda (string) (untokenize-symbol package-name internal-p string))
          (sort strings #'string<)))




;;; Fancy "flex" completion
;;; 
(defun score-completion (indexes short full)
  "Return a float, between 0 and 1, that represents the likelihood that the
SHORT string refers to with the FULL string. The higher the better.

INDEXES: As list of integers, as returned by FLEX-MATCHES.
SHORT: An abbreviation used as a pattern for the completion.
FULL: A candidate for completion. "
  (declare (ignore short))
  (float
   (/ 1
      (* (length full)
         (max 1
              (reduce #'+
                      (loop for (a b) on indexes
                            while b
                            collect (- b a 1))))))))


(defmacro collecting ((&rest collectors) &body body) ; lifted from uiop
  "COLLECTORS should be a list of names for collections.  A collector
defines a function that, when applied to an argument inside BODY, will
add its argument to the corresponding collection.  Returns multiple values,
a list for each collection, in order.
   E.g.,
\(collecting \(foo bar\)
           \(dolist \(x '\(\(a 1\) \(b 2\) \(c 3\)\)\)
             \(foo \(first x\)\)
             \(bar \(second x\)\)\)\)
Returns two values: \(A B C\) and \(1 2 3\)."
  (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
        (initial-values (mapcar (constantly nil) collectors)))
    `(let ,(mapcar #'list vars initial-values)
       (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v) (values))) collectors vars)
         ,@body
         (values ,@(mapcar #'(lambda (v) `(reverse ,v)) vars))))))

(defun to-chunks (string indexes)
  "Return a list of pairs. Each pair is made by the index and the character
found in the STRING of the position referenced by the index. The character is
presented a string of length 1.

STRING: The candidate for completion.
INDEXES: The indexes as returned by FLEX-MATCHES."
  (reverse (reduce (lambda (chunk-list number)
                     (if (and chunk-list
                              (= (1+ (first (first chunk-list)))
                                 number))
                         (progn (setf (second (first chunk-list))
                                      (format nil "~a~c" (second (first chunk-list))
                                              (aref string number)))
                                chunk-list)
                         (cons (list number (format nil "~c" (aref string number)))
                               chunk-list)))
                   indexes
                   :initial-value nil)))

(defun flex-matches (pattern string)
  "Return a list of the positions where each character of the PATTERN is first
found on STRING."
  (let ((indexes (loop for char across pattern
                       for from = 0 then (1+ pos)
                       for pos = (position char string :start from :test #'char-equal)
                       unless pos
                         return nil
                       collect pos)))
    indexes))

(defun collect-maybe (collector pattern string symbol)
  "When STRING matches PATTERN, collect the match object using COLLECTOR."
  (when-let (indexes (flex-matches pattern string))
    (funcall collector
             (list string
                   symbol
                   indexes
                   (score-completion indexes
                                     pattern
                                     string)))))

(declaim (inline completion-score))
(defun completion-score (match)
  "Return the completion score of the MATCH object."
  (fourth match))

(defun sort-by-score (matches)
  "Sort MATCHES by completion score."
  (sort matches #'> :key #'completion-score))

(defun keywords-matching (pattern)
  "Returns the matches of the PATTERN against the symbols in the keyword
package."
  (collecting (collect)
    (and (char= (aref pattern 0) #\:)
         (do-symbols (s (find-package :keyword))
           (collect-maybe #'collect pattern (format nil ":~a" (symbol-name s)) s)))))

(defun accessible-matching (pattern package)
  "Returns the matches of the PATTERN against all symbols accessible from
PACKAGE."
  (and (not (find #\: pattern))        ; Make sure the pattern does not contain a package prefix. (Package marker ?)
       (collecting (collect)
         (do-symbols (s package)
           (collect-maybe #'collect pattern (symbol-name s) s)))))

;;; TODO: Add SBCL specific version for package local nicknames
(defun preferred-package-name (package)
  "Return the preferred string representation to refer to a package. The
shorter the name or nickname, the better."
  (first (sort (cons (package-name package)
                     (copy-list (package-nicknames package)))
               #'<
               :key #'length)))

(defun symbol-information (symbol)
  "Return the symbol, the preferred name for the package and a boolean
representing if the symbol is external or not in said package. If symbol is an
apparently uninterned symbol return NIL."
  (when-let (symbol-package (symbol-package symbol))
    (values (symbol-name symbol)
            (preferred-package-name symbol-package)
            (slynk::symbol-external-p symbol))))

(defun qualified-matching-1 (pattern package)
  "Match the PATTERN against all symbols in PACKAGE.

Return two values, The list of matches of the external symbols and a list of
matches of the internal symbols."
  (collecting (collect-external collect-internal)
    (do-symbols (symbol package)
      (multiple-value-bind
            (symbol-name symbol-package externalp) (symbol-information symbol)
        (if externalp
            ;; XXX: Consider factoring both calls to formats below
            (collect-maybe #'collect-external pattern (format nil "~A:~A"  symbol-package symbol-name) symbol)
            (collect-maybe #'collect-internal pattern (format nil "~A::~A" symbol-package symbol-name) symbol))))))

(defun qualified-matching (pattern &optional package)
  "Match the PATTERN against all symbols in PACKAGE. If package is nil match
the PATTERN against all the symbols with a home package. This excludes
apparently uninterned symbols.

 Returns a list of matches, where each match is a list of the string resulting
 of printing the symbol, the symbol, the indexes and the matching score."
  (and (not (char= (aref pattern 0) #\:))
       (if package
           (qualified-matching-1 pattern package)
           (mappend #'(lambda (package) (qualified-matching-1 pattern package))
                    (list-all-packages)))))

(defslyfun flex-completions (pattern package-name &key (limit 300))
  "Return \"flex\"completions for PATTERN.
Returns a list of (COMPLETIONS NIL). COMPLETIONS is a list of
\(STRING SCORE CHUNKS CLASSIFICATION-STRING)."
  (when (plusp (length pattern))
    (let ((convert (if (every #'common-lisp:upper-case-p pattern)
                       #'string-upcase
                       #'string-downcase)))
      (list (loop 
              with package = (guess-buffer-package package-name)
              for (string symbol indexes score)
                in
                (remove-duplicates
                 (multiple-value-bind (external internal) (qualified-matching pattern package)
                   (loop for e in (append (sort-by-score
                                           (keywords-matching pattern))
                                          (sort-by-score
                                           (append (accessible-matching pattern package)
                                                   external))
                                          (sort-by-score
                                           internal))
                         for i upto limit
                         collect e))
                 :from-end t
                 :test #'string=
                 :key #'first)
              collect
              (list (funcall convert string)
                    score
                    (to-chunks string indexes)
                    (slynk::symbol-classification-string symbol)))
            nil))))

(provide :slynk-completion)
