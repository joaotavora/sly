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
(defun score-completion (indexes short full)
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
  (let ((indexes (loop for char across pattern
        for from = 0 then (1+ pos)
        for pos = (position char string :start from :test #'char-equal)
        unless pos
          return nil
        collect pos)))
    indexes))

(defun collect-maybe (collector pattern string symbol)
  (let ((indexes (flex-matches pattern string)))
    (when indexes
      (funcall collector
               (list string
                     symbol
                     indexes
                     (score-completion indexes
                                       pattern
                                       string))))))

(defun sort-by-score (what)
  (sort what #'> :key #'fourth))

(defun keywords-matching (pattern)
  (collecting (collect)
    (and (char= (aref pattern 0) #\:)
         (do-symbols (s (find-package :keyword))
           (collect-maybe #'collect pattern (format nil ":~a" (symbol-name s)) s)))))

(defun accessible-matching (pattern package)
  (and (not (find #\: pattern))
       (collecting (collect)
         (do-symbols (s package)
           (collect-maybe #'collect pattern (symbol-name s) s)))))

(defun qualified-matching (pattern package)
  (declare (ignore package))
  (and (not (char= (aref pattern 0) #\:))
       (collecting (collect-external collect-internal)
         (do-all-symbols (s)
           (slynk-backend:when-let (symbol-package (symbol-package s))
             (let* ((nicknames (package-nicknames symbol-package))
                    (sorted-nicknames (sort (cons (package-name symbol-package)
                                                  (copy-list nicknames))
                                            #'<
                                            :key #'length)))
               (when (and (not (excluded-from-searches-p s))
                          ;; keyword symbols are handled explicitly by
                          ;; `keyword-matching', so don't repeat them here.
                          ;; 
                          (and (not (eq (find-package :keyword)
                                        symbol-package))))
                 (loop ;; TODO: add package-local nicknames: `package' might
                       ;; know `symbol-package' under more nicknames. They
                       ;; should be added and perhaps also sorted according to
                       ;; length.
                       ;;
                       for nickname in sorted-nicknames
                       for external-p = (slynk::symbol-external-p s)
                       do
                          (cond (external-p
                                 (collect-maybe #'collect-external
                                                pattern
                                                (format nil "~a:~a"
                                                        nickname
                                                        (symbol-name s))
                                                s))
                                (t
                                 (collect-maybe #'collect-internal
                                                pattern
                                                (format nil "~a::~a"
                                                        nickname
                                                        (symbol-name s))
                                                s)))))))))))

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
              (list (funcall convert string)
                    score
                    (to-chunks string indexes)
                    (slynk::symbol-classification-string symbol)))
            nil))))

(provide :slynk-completion)
