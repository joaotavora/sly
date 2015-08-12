;;; sly-named-readtables.el --- Automatically parse in-readtable forms in Lisp buffers  -*- lexical-binding: t; -*-
;; Copyright (C) 2015  João Távora

(define-sly-contrib sly-named-readtables
  "Automatically parse in-readtable forms in Lisp buffers"
  (:slynk-dependencies slynk-named-readtables)
  (:on-load )
  (:on-unload ))

(defun sly-named-readtables--grok-current-table ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(named-readtables:\\)?in-readtable\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (re-search-backward regexp nil t)
        (match-string-no-properties 2)))))

;; (add-to-list 'sly-mode-line-format `(:eval (sly-named-readtables--mode-line-format)) 'append)

(defun sly-named-readtables--mode-line-format ()
   `((,(sly-named-readtables--grok-current-table))))


(provide 'sly-named-readtables)
;;; sly-named-readtables.el ends here

