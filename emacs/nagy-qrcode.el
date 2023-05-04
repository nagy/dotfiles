;;; nagy-qrcode.el --- My qrcode config -*- lexical-binding: t -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") dash anaphora)

(eval-when-compile
  (require 'cl-lib))

(require 'dash)
(require 'xml)
(require 'dom)
(require 'anaphora)

(defun nagy-qr-datatag-todata (data-tag)
  (if (cadr data-tag)
      (base64-decode-string (caddr data-tag))
    (caddr data-tag)))

(defun nagy-qr-list-data (x)
  (-map #'nagy-qr-datatag-todata (dom-by-tag x 'data)))

(cl-defun nagy-qr-raw-zbarimg-output (&optional (filename (buffer-file-name)))
  (with-temp-buffer
    (when (zerop (call-process "zbarimg" filename t nil "--quiet" "--nodbus" "--xml" "-"))
      (goto-char (point-min))
      (xml-parse-tag-1))))

(defun nagy-image-scan-qrcode ()
  (nagy-qr-list-data (nagy-qr-raw-zbarimg-output)))

(defun nagy-qr-choose (results)
  (interactive)
  (cl-case (length results)
    (1 (car results))
    (0 nil)
    (t (completing-read "QR Code: " (nagy-image-scan-qrcode)))))

(defun kill-qrcode ()
  (interactive)
  (let ((chosen (nagy-qr-choose (nagy-image-scan-qrcode))))
    (if chosen
        (message "QR-Code: %S" (kill-new chosen))
      (error "No QR-Code found"))))

(defun qr-take-screenshot-and-kill ()
  (interactive)
  (when (fboundp 'take-screenshot)
    (alet (take-screenshot)
      (with-current-buffer (find-file-noselect it)
        (kill-qrcode)))))

(provide 'nagy-qrcode)
;;; nagy-qrcode.el ends here
