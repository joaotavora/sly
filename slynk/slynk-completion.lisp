;;; slynk-flex-completion.lisp --- Common Lisp symbol completion routines
;;
;; Authors: João Távora, some parts derivative works of SLIME, by its
;; authors.
;;
(defpackage :slynk-completion
  (:use #:cl #:slynk-api)
  (:export
   #:flex-completions
   #:simple-completions))

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
  "Return chunks of STRING in as specified by INDEXES."
  ;; (to-chunks "farfalhini" '(1 2 3 4))           => ((1 "arfa"))
  ;; (to-chunks "farfalhini" '(1 3 4))             => ((1 "a") (3 "fa"))
  ;; (to-chunks "farfalhini" '(1 2 3 4 5 7 8 9))   => ((1 "arfal") (7 "ini"))
  ;; (to-chunks "farfalhini" '(1 2 3 4 5 6 7 8 9)) => ((1 "arfalhini"))
  (reverse (reduce (lambda (chunk-list number)
                     (let ((latest-chunk (car chunk-list)))
                       (if (and latest-chunk
                                (= (+
                                    (length (second latest-chunk))
                                    (first latest-chunk))
                                   number))
                           (progn (setf (second latest-chunk)
                                        (format nil "~a~c" (second latest-chunk)
                                                (aref string number)))
                                  chunk-list)
                           (cons (list number (format nil "~c" (aref string number)))
                                 chunk-list))))
                   indexes
                   :initial-value nil)))

(defun flex-score (pattern string symbol indexes)
  "Score the match of PATTERN on STRING.
INDEXES as calculated by FLEX-MATCHES"
  ;; FIXME: hideously poor scoring
  (declare (ignore pattern symbol))
  (float
   (/ 1
      (* (length string)
         (max 1
              (reduce #'+
                      (loop for (a b) on indexes
                            while b
                            collect (- b a 1))))))))

(defun flex-matches (pattern string symbol)
  "Return non-NIL if PATTERN flex-matches STRING.
In case of a match, return two values:

A list of non-negative integers which are the indexes of the
characters in PATTERN as found consecutively in STRING. This list
measures in length the number of characters in PATTERN.

A floating-point score. Higher scores for better matches."
  (let ((indexes (loop for char across pattern
                       for from = 0 then (1+ pos)
                       for pos = (position char string :start from :test #'char-equal)
                       unless pos
                         return nil
                       collect pos)))
    (values indexes
            (and indexes
                 (flex-score pattern string symbol indexes)))))

(defun collect-if-matches (collector pattern string symbol)
  "Make and collect a match with COLLECTOR if PATTERN matches STRING.
A match is a list (STRING SYMBOL INDEXES SCORE)."
  (multiple-value-bind (indexes score)
      (flex-matches pattern string symbol)
    (when indexes
      (funcall collector
               (list string
                     symbol
                     indexes
                     score)))))

(defun sort-by-score (matches)
  "Sort MATCHES by SCORE, highest score first.

Matches are produced by COLLECT-IF-MATCHES (which see)."
  (sort matches #'> :key #'fourth))

(defun keywords-matching (pattern)
  "Find keyword symbols flex-matching PATTERN.
Return an unsorted list of matches.

Matches are produced by COLLECT-IF-MATCHES (which see)."
  (collecting (collect)
    (and (char= (aref pattern 0) #\:)
         (do-symbols (s (find-package :keyword))
           (collect-if-matches #'collect pattern (format nil ":~a" (symbol-name s)) s)))))

(defun accessible-matching (pattern package)
  "Find symbols flex-matching PATTERN accessible without package-qualification.
Return an unsorted list of matches.

Matches are produced by COLLECT-IF-MATCHES (which see)."
  (and (not (find #\: pattern))
       (collecting (collect)
         (do-symbols (s package)
           (collect-if-matches #'collect pattern (symbol-name s) s)))))

(defun qualified-matching (pattern home-package)
  "Find package-qualified symbols flex-matching PATTERN.
Return, as two values, a set of matches for external symbols,
package-qualified using one colon, and another one for internal
symbols, package-qualified using two colons.

The matches in the two sets are not guaranteed to be in their final
order, i.e. they are not sorted (except for the fact that
qualifications with shorter package nicknames are tried first).

Matches are produced by COLLECT-IF-MATCHES (which see)."
  (and
   (not (char= (aref pattern 0) #\:))
   (let ((package-local-nicknames
           (slynk-backend:package-local-nicknames home-package))
         (sorted-nicknames-by-package (make-hash-table)))
     (flet ((sorted-nicknames (package)
              (or (gethash package sorted-nicknames-by-package)
                  (setf (gethash package sorted-nicknames-by-package)
                        (sort (append
                               (loop for (short . full) in
                                     package-local-nicknames
                                     when (eq (find-package full)
                                              package)
                                       collect short)
                               (copy-list (package-nicknames package))
                               (list (package-name package)))
                              #'<
                              :key #'length)))))
       (collecting (collect-external collect-internal)
         (do-all-symbols (s)
           (slynk-backend:when-let (symbol-package (symbol-package s))
             (when (and (not (excluded-from-searches-p s))
                        ;; keyword symbols are handled explicitly by
                        ;; `keyword-matching', so don't repeat them here.
                        ;;
                        (and (not (eq (find-package :keyword)
                                      symbol-package))))
               (loop
                 for nickname in (sorted-nicknames symbol-package)
                 for external-p = (slynk::symbol-external-p s)
                 do
                    (cond (external-p
                           (collect-if-matches #'collect-external
                                               pattern
                                               (format nil "~a:~a"
                                                       nickname
                                                       (symbol-name s))
                                               s))
                          (t
                           (collect-if-matches #'collect-internal
                                               pattern
                                               (format nil "~a::~a"
                                                       nickname
                                                       (symbol-name s))
                                               s))))))))))))

(defslyfun flex-completions (pattern package-name &key (limit 300))
  "Compute \"flex\" completions for PATTERN given current PACKAGE-NAME.
  Returns a list of (COMPLETIONS NIL). COMPLETIONS is a list of
  \(STRING SCORE CHUNKS CLASSIFICATION-STRING)."
  (when (plusp (length pattern))
    (list (loop
            with package = (guess-buffer-package package-name)
            for (string symbol indexes score)
              in
              (remove-duplicates
               (loop with (external internal)
                       = (multiple-value-list (qualified-matching pattern package))
                     for e in (append (sort-by-score
                                       (keywords-matching pattern))
                                      (sort-by-score
                                       (append (accessible-matching pattern package)
                                               external))
                                      (sort-by-score
                                       internal))
                     for i upto limit
                     collect e)
               :from-end t
               :test #'string=
               :key #'first)
            collect
            (list (if (every #'common-lisp:upper-case-p pattern)
                      (string-upcase string)
                      (string-downcase string))
                  score
                  (to-chunks string indexes)
                  (slynk::symbol-classification-string symbol)))
          nil)))

(provide :slynk-completion)
