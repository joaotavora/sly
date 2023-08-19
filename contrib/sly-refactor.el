;;; -*- coding: utf-8; lexical-binding: t -*-
;;;
;;; sly-refactor.el -- refactoring shortcuts
;;;

(define-sly-contrib sly-refactor
  "Refactor threading macros"
  (:authors "Almost entirely copied from clojure-mode and clj-refactor")
  (:license "GPL"))

(require 'sly)

(defun sly--fix-sexp-whitespace (&optional move-out)
  "Fix whitespace after unwinding a threading form.

Optional argument MOVE-OUT, if non-nil, means moves up a list
before fixing whitespace."
  (save-excursion
    (when move-out (backward-up-list))
    (let ((sexp (bounds-of-thing-at-point 'sexp)))
      (indent-region (car sexp) (cdr sexp))
      (delete-trailing-whitespace (car sexp) (cdr sexp)))))

(defun sly--pop-out-of-threading ()
  "Raise a sexp up a level to unwind a threading form."
  (save-excursion
    (down-list 2)
    (backward-up-list)
    (raise-sexp)))

(defun sly--nothing-more-to-unwind ()
  "Return non-nil if a threaded form cannot be unwound further."
  (save-excursion
    (let ((beg (point)))
      (forward-sexp)
      (down-list -1)
      (backward-sexp 2) ;; the last sexp, the threading macro
      (when (looking-back "(\\s-*" (line-beginning-position))
        (backward-up-list)) ;; and the paren
      (= beg (point)))))

(defun sly-delete-and-extract-sexp ()
  "Delete the surrounding sexp and return it."
  (let ((begin (point)))
    (forward-sexp)
    (let ((result (buffer-substring begin (point))))
      (delete-region begin (point))
      result)))

(defun sly--maybe-unjoin-line ()
  "Undo a `join-line' done by a threading command."
  (when (get-text-property (point) 'sly-thread-line-joined)
    (remove-text-properties (point) (1+ (point)) '(sly-thread-line-joined t))
    (insert "\n")))

(defun sly--ensure-parens-around-function-names ()
  "Insert parens around function names if necessary."
  (comment-normalize-vars)
  (comment-forward (point-max))
  (unless (looking-at "(")
    (insert-parentheses 1)
    (let ((lined-joined-p (remove-text-properties (point) (1+ (point)) '(sly-thread-line-joined t))))
      (backward-up-list)
      (when lined-joined-p
        (put-text-property (point) (1+ (point))
                           'sly-thread-line-joined t)))))

(defun sly--unwind-first ()
  "Unwind a thread first macro once.

Point must be between the opening paren and the -> or ~> symbol."
  (forward-sexp)
  (let ((contents (sly-delete-and-extract-sexp)))
    (when (looking-at " *\n")
      (join-line 'following))
    (sly--ensure-parens-around-function-names)
    (down-list)
    (forward-sexp)
    (save-excursion (sly--maybe-unjoin-line))
    (save-excursion (insert contents))
    (forward-char)
    (sly--maybe-unjoin-line)))

(defun sly--unwind-last ()
  "Unwind a thread last macro once.

Point must be between the opening paren and the ->> or ~>> symbol."
  (forward-sexp)
  (let ((contents (sly-delete-and-extract-sexp)))
    (when (looking-at " *\n")
      (join-line 'following))
    (sly--ensure-parens-around-function-names)
    (let* ((sexp-beg-line (line-number-at-pos))
           (sexp-end-line (progn (forward-sexp)
                                 (line-number-at-pos)))
           (multiline-sexp-p (not (= sexp-beg-line sexp-end-line))))
      (down-list -1)
      (if multiline-sexp-p
          (insert "\n")
        ;; `sly--maybe-unjoin-line' only works when unwinding sexps that were
        ;; threaded in the same Emacs session, but it also catches cases that
        ;; `multiline-sexp-p' doesn't.
        (sly--maybe-unjoin-line))
      (insert contents))))

(defun sly-unwind (&optional n)
  "Unwind thread at point or above point by N levels.
With universal argument \\[universal-argument], fully unwind thread."
  (interactive "P")
  (setq n (cond ((equal n '(4)) 999)
                (n)
                (1)))
  (save-excursion
    (let ((limit (save-excursion
                   (beginning-of-defun)
                   (point))))
      (ignore-errors
        (when (looking-at "[(-~>]")
          (forward-char 1)
          (forward-sexp 1)))
      (while (> n 0)
        (search-backward-regexp "([^-~]*[-~]>" limit)
        (if (sly--nothing-more-to-unwind)
            (progn (sly--pop-out-of-threading)
                   (sly--fix-sexp-whitespace)
                   (setq n 0)) ;; break out of loop
          (down-list)
          (cond
           ((looking-at "[^-~]*[-~]>\\_>")  (sly--unwind-first))
           ((looking-at "[^-~]*[-~]>>\\_>") (sly--unwind-last)))
          (sly--fix-sexp-whitespace 'move-out)
          (setq n (1- n)))))))

(defun sly--remove-superfluous-parens ()
  "Remove extra parens from a form."
  (when (looking-at "([^ )]+)")
    (let ((delete-pair-blink-delay 0)
          (lined-joined-p (get-text-property (point) 'sly-thread-line-joined)))
      (delete-pair)
      (when lined-joined-p
        (put-text-property (point) (1+ (point))
                           'sly-thread-line-joined t)))))

(defun sly--thread-first ()
  "Thread a nested sexp using ->."
  (down-list)
  (forward-symbol 1)
  (unless (looking-at ")")
    (let ((contents (sly-delete-and-extract-sexp)))
      (when (looking-at "\\s-*\n")
        (join-line 'following)
        (put-text-property (point) (1+ (point))
                           'sly-thread-line-joined t))
      (backward-up-list)
      (just-one-space 0)
      (save-excursion
        (insert contents "\n")
        (sly--remove-superfluous-parens))
      (when (looking-at "\\s-*\n")
        (join-line 'following)
        (forward-char 1)
        (put-text-property (point) (1+ (point))
                           'sly-thread-line-joined t))
      t)))

(defun sly--thread-last ()
  "Thread a nested sexp using ->> or ~>>."
  (forward-sexp 2)
  (down-list -1)
  (backward-sexp)
  (unless (eq (char-before) ?\()
    (let ((contents (sly-delete-and-extract-sexp)))
      (just-one-space 0)
      (backward-up-list)
      (insert contents "\n")
      (sly--remove-superfluous-parens)
      ;; cljr #255 Fix dangling parens
      (forward-sexp)
      (when (looking-back "^\\s-*\\()+\\)\\s-*" (line-beginning-position))
        (let ((pos (match-beginning 1)))
          (put-text-property pos (1+ pos) 'sly-thread-line-joined t))
        (join-line))
      t)))

(defun sly--threadable-p ()
  "Return non-nil if a form can be threaded."
  (save-excursion
    (forward-symbol 1)
    (looking-at "[\n\r\t ]*(")))

(defun sly-thread ()
  "Thread by one more level an existing threading macro."
  (interactive)
  (ignore-errors
    (when (looking-at "[(-~>]")
      (forward-char 1)
      (forward-sexp 1)))
  (search-backward-regexp "([^-~]*[-~]>")
  (down-list)
  (when (sly--threadable-p)
    (prog1 (cond
            ((looking-at "[^-~]*[-~]>\\_>")  (sly--thread-first))
            ((looking-at "[^-~]*[-~]>>\\_>") (sly--thread-last)))
      (sly--fix-sexp-whitespace 'move-out))))

(defun sly--thread-all (first-or-last-thread)
  "Fully thread the form at point.

FIRST-OR-LAST-THREAD is \"->\" or \"->>\", \"~>\" or \"~>>\"."
  (unless (looking-at "(")
      (backward-up-list))
  (save-excursion
    (insert-parentheses 1)
    (insert first-or-last-thread))
  (while (save-excursion (sly-thread))))

(defcustom sly-threading-macro "->"
  "Whether to use -> or ~> style threading macros for refactoring commands"
  :type 'string
  :options '("->" "~>")
  :group 'sly)

(defun sly-thread-first-all ()
  "Fully thread the form at point using -> or ~>."
  (interactive)
  (sly--thread-all (concat sly-threading-macro " ")))

(defun sly-thread-last-all ()
  "Fully thread the form at point using ->> or ~>>."
  (interactive)
  (sly--thread-all (concat sly-threading-macro "> ")))

(defun sly-unwind-all ()
  "Fully unwind thread at point or above point."
  (interactive)
  (sly-unwind '(4)))

(provide 'sly-refactor)
