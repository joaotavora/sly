;;; sly-autoloads.el --- autoload definitions for SLY

;; Copyright (C) 2007  Helmut Eller

;; This file is protected by the GNU GPLv2 (or later), as distributed
;; with GNU Emacs.

;;; Commentary:

;; This code defines the necessary autoloads, so that we don't need to
;; load everything from .emacs.
;;
;; JT@14/01/09: FIXME: This file should be auto-generated with autoload cookies.

;;; Code:

(autoload 'sly "sly"
  "Start a Lisp subprocess and connect to its Slynk server." t)

(autoload 'sly-mode "sly"
  "SLY: The Superior Lisp Interaction (Minor) Mode for Emacs." t)

(autoload 'sly-connect "sly"
  "Connect to a running Slynk server." t)

(autoload 'hyperspec-lookup "lib/hyperspec" nil t)

(autoload 'sly-editing-mode "sly" "SLY" t)

(defvar sly-contribs '(sly-fancy)
  "A list of contrib packages to load with SLY.")

(autoload 'sly-setup "sly"
  "Setup some SLY contribs.")

(define-obsolete-variable-alias 'sly-setup-contribs
  'sly-contribs "2.3.2")

(if (or (not (memq 'slime-lisp-mode-hook lisp-mode-hook))
        noninteractive
        (prog1 (y-or-n-p "[sly] SLIME detected in `lisp-mode-hook', which causes keybinding conflicts.
Remove it for this Emacs session?")
          (warn
           "To restore SLIME in this session, customize `lisp-mode-hook'
and replace `sly-editing-mode' with `slime-lisp-mode-hook'.")
          (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)))
    (add-hook 'lisp-mode-hook 'sly-editing-mode)
  (warn "`sly.el' loaded OK. To use SLY, customize `lisp-mode-hook' and
replace `slime-lisp-mode-hook' with `sly-editing-mode'."))

(provide 'sly-autoloads)

;;; sly-autoloads.el ends here
;; Local Variables:
;; no-byte-compile: t
;; End:
