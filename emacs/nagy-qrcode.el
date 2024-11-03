;;; nagy-qrcode.el --- Qrcode config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") anaphora)

;; (eval-when-compile
;;   (require 'anaphora)
;;   (require 'cl-lib))

;; (require 'dom)
;; (require 'image-mode)
;; (require 'xml)

(declare-function xml-parse-tag-1 "xml")
(declare-function dom-by-tag "dom")
(declare-function image-transform-fit-to-window "image-mode")

(defun nagy-qrcode-datatag-todata (data-tag)
  (if (cadr data-tag)
      (base64-decode-string (caddr data-tag))
    (caddr data-tag)))

(defun nagy-qrcode-list-data (x)
  (mapcar #'nagy-qrcode-datatag-todata (dom-by-tag x 'data)))

(cl-defun nagy-qrcode-raw-zbarimg-output (&optional (filename (buffer-file-name)))
  (with-temp-buffer
    (cl-assert (zerop (call-process "zbarimg" filename t nil "--quiet" "--nodbus" "--xml" "-")))
    (goto-char (point-min))
    (xml-parse-tag-1)))

(defun nagy-qrcode-image-scan ()
  (nagy-qrcode-list-data (nagy-qrcode-raw-zbarimg-output)))

(defun nagy-qrcode-choose (results)
  (interactive)
  (pcase (length results)
    (1 (car results))
    (0 nil)
    (_ (completing-read "QR Code: " (nagy-qrcode-image-scan)))))

(defun nagy-qrcode-kill ()
  (interactive)
  (aif (nagy-qrcode-choose (nagy-qrcode-image-scan))
      (message "QR-Code: %S" (kill-new it))
    (error "No QR-Code found")))

(declare-function take-screenshot "nagy-misc2")

;;;###autoload
(defun qr-take-screenshot-and-kill ()
  (interactive)
  (alet (take-screenshot)
    (with-current-buffer (find-file-noselect it)
      (nagy-qrcode-kill))))

;;;###autoload
(defun qrcode-region (start end)
  "Show a QR code of the region between START and END.
A simple way to transfer text to the phone."
  (interactive (list (if (region-active-p) (region-beginning) (point-min))
                     (if (region-active-p) (region-end) (point-max))))
  (let ((buf (generate-new-buffer "*qr*")))
    (let ((coding-system-for-read 'raw-text))
      (shell-command-on-region start end "qrencode --output=-" buf))
    (with-current-buffer buf
      ;; (after-find-file ...)
      (set-auto-mode)
      (image-transform-fit-to-window))
    (switch-to-buffer buf)))

(provide 'nagy-qrcode)
;;; nagy-qrcode.el ends here
