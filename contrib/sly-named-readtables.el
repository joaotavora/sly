;;; sly-named-readtables.el --- Automatically parse in-readtable forms in Lisp buffers  -*- lexical-binding: t; -*-
;; Copyright (C) 2015  João Távora

(define-sly-contrib sly-named-readtables
  "Automatically parse in-readtable forms in Lisp buffers"
  (:slynk-dependencies slynk-named-readtables)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-named-readtables-mode))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-named-readtables-mode)))

(defun sly-named-readtable--pretty-name (name)
  (cond ((string-match "^#?:\\(.*\\)$" name)
         (match-string 1 name))
        ((string-match "^\"\\(.*\\)\"$" name)
         (match-string 1 name))
        (t name)))

(define-minor-mode sly-named-readtables-mode
  "Use EDITOR-HINTS.NAMED-READTABLES if available."
  nil nil nil
  (cond (sly-named-readtables-mode
         (add-to-list 'sly-extra-mode-line-constructs
                      'sly-named-readtables--mode-line-construct
                      t)
         (add-to-list 'sly-rex-extra-options-functions
                      'sly-named-readtables--pass-readtable
                      t))
        (t
         (setq sly-extra-mode-line-constructs
               (delq 'sly-named-readtables--mode-line-construct
                     sly-extra-mode-line-constructs)
               sly-rex-extra-options-functions
               (deql 'sly-named-readtables--pass-readtable
                     sly-rex-extra-options-functions)))))

(defun sly-named-readtables--grok-current-table ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(named-readtables:\\)?in-readtable\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (re-search-backward regexp nil t)
        (match-string-no-properties 2)))))

(defun sly-named-readtables--mode-line-construct ()
  (let ((readtable-name (sly-named-readtables--grok-current-table)))
    `(:propertize ,(or (and readtable-name
                            (sly-named-readtable--pretty-name readtable-name))
                       "*")
                  face ,(if readtable-name 'hi-pink 'sly-mode-line)
                  mouse-face mode-line-highlight
                  help-echo ,(if readtable-name
                                 (format "Special NAMED-READTABLE %s" readtable-name)
                               "Default readtable"))))

(defun sly-named-readtables--pass-readtable ()
  (list :named-readtable (sly-named-readtables--grok-current-table)))

(provide 'sly-named-readtables)
;;; sly-named-readtables.el ends here

