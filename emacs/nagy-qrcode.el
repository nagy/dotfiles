;;; nagy-qrcode.el --- My qrcode config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") anaphora)

(eval-when-compile
  (require 'cl-lib))

(require 'dom)
(require 'xml)

(require 'anaphora)

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
  (cl-case (length results)
    (1 (car results))
    (0 nil)
    (t (completing-read "QR Code: " (nagy-qrcode-image-scan)))))

(defun nagy-qrcode-kill ()
  (interactive)
  (let ((chosen (nagy-qrcode-choose (nagy-qrcode-image-scan))))
    (if chosen
        (message "QR-Code: %S" (kill-new chosen))
      (error "No QR-Code found"))))

(defun qr-take-screenshot-and-kill ()
  (interactive)
  (when (fboundp 'take-screenshot)
    (alet (take-screenshot)
      (with-current-buffer (find-file-noselect it)
        (nagy-qrcode-kill)))))

(provide 'nagy-qrcode)
;;; nagy-qrcode.el ends here
