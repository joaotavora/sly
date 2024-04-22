;;; -*-lexical-binding:t-*-
(require 'sly)
(require 'cl-lib)

(define-sly-contrib sly-media
  "Display images in SLY buffers"
  (:authors "Jin-Cheng Guu <jcguu95@gmail.com>")
  (:license "GPL")
  ;; (:on-load
  ;;  (add-hook 'sly-event-hooks 'sly-dispatch-media-event))
  )

(defun sly-media-insert-image (image string)
  "Insert image into the current sly buffer. Example Usage:
(1) Define function in the Lisp that connects to the current sly buffer.
    ; (defun display-image-on-disk (filename)
    ;   \"Displays an image in the current sly buffer.\"
    ;   (slynk:eval-in-emacs
    ;    `(sly-media-insert-image
    ;      (create-image ,filename) ,filename)))
(2) Evaluate in emacs: (setq sly-enable-evaluate-in-emacs t)
(3) Evaluate in the sly buffer: (display-image-on-disk \"/tmp/example.jpg\")
"
  (with-current-buffer
      (get-buffer (sly-buffer-name :mrepl :connection (sly-current-connection)))
    (let ((marker (sly-mrepl--mark)))
      (goto-char marker)
      (sly-propertize-region `(face slime-mrepl-output-face
                               rear-nonsticky (face))
        (insert-image image string))
      (set-marker marker (point)))
    nil))

;; (defun sly-dispatch-media-event (event)
;;   (sly-dcase event
;;     ((:write-image image string)
;;      (cl-labels
;;          ((media-decode-image (image)
;;             (mapcar (lambda (image)
;;                       (if (plist-get image :data)
;;                           (plist-put image :data (base64-decode-string
;;                                                   (plist-get image :data)))
;;                         image))
;;                     image)))
;;        (let ((img (or (find-image (media-decode-image image))
;;                       (create-image image))))
;;          (sly-media-insert-image img string)))
;;      t)
;;     (t nil)))

(provide 'sly-media)
