;;; nagy-formats.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: January 27, 2023
;; Modified: January 27, 2023
;; Version: 0.0.1
;; Keywords: convenience, files
;; Homepage: https://github.com/nagy/nagy-formats
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'format)
(require 'yaml-mode)
(require 'dired)
;; (require 'hi-lock)
(eval-when-compile
  (require 'cl-lib))

(defvar-local nagy-formats--base-buffer nil)
;;  maybe permanent local ?

;; Modification of original function to use generate-new-buffer because the
;; original used get-buffer-create and that had problems with the kill hook
;; below, that as also been removed. This leaves dangling buffers around. A nice
;; solution to this kind of problem would be anonymous buffers, that could get
;; garbage collected.
(defun format-annotate-function (format from to orig-buf format-count)
  (let* ((info (assq format format-alist))
         (to-fn  (nth 4 info))
         (modify (nth 5 info)))
    (if to-fn
        (if modify
            (let ((copy-buf (generate-new-buffer (format " *Format Temp %d*" format-count)))
                  (sel-disp selective-display)
                  (multibyte enable-multibyte-characters)
                  (coding-system buffer-file-coding-system))
              (with-current-buffer copy-buf
                (setq selective-display sel-disp)
                (set-buffer-multibyte multibyte)
                (setq buffer-file-coding-system coding-system))
              (let ((inhibit-read-only t)) ; bug#14887
                (copy-to-buffer copy-buf from to)
                (set-buffer copy-buf)
                (format-insert-annotations write-region-annotations-so-far from)
                (format-encode-run-method to-fn (point-min) (point-max) orig-buf))
              nil)
          (funcall to-fn from to orig-buf)))))

;; Copy of original function. Needed to add inhibit-read-only. might be possible
;; with an advice.
(defun format-decode-run-method (method from to &optional _buffer)
  (cl-assert (stringp method))
  (let ((error-buff (generate-new-buffer "*Format Errors*"))
        (coding-system-for-write 'no-conversion)
        (inhibit-read-only t)
        format-alist)
    (cl-assert (and (zerop (save-window-excursion
                             (shell-command-on-region from to method t 'no-mark error-buff)))
                    (zerop (with-current-buffer error-buff
                             (buffer-size)))))
    (point)))


(add-to-list 'format-alist '(pcap2txt "pcap2txt" nil ",pcap,txt" nil nil nil))
(add-to-list 'format-alist '(pcap2json "pcap2json" nil ",pcap,json" nil nil nil))
(add-to-list 'format-alist '(pcap2xml "pcap2xml" nil ",pcap,xml" nil nil nil))

(define-derived-mode pcap-mode fundamental-mode "PCAP"
  "hello"
  (if (derived-mode-p 'pcap-mode)
      (progn
        (format-decode-buffer '(pcap2txt))
        (highlight-lines-matching-regexp "ICMP" 'modus-themes-intense-magenta)
        (highlight-lines-matching-regexp "TCP" 'modus-themes-intense-blue)
        (highlight-lines-matching-regexp "HTTP" 'modus-themes-intense-green)
        (highlight-lines-matching-regexp (rx (or "UDP" "NTP")) 'modus-themes-intense-cyan)
        (highlight-lines-matching-regexp "RST" 'modus-themes-intense-red))))
(add-to-list 'auto-mode-alist '("\\.pcap\\'" . pcap-mode))

(defun nagy-formats--call-converter (from to)
  (let ((result (nagy-formats-convert from to)))
    (cl-typecase result
      (string (shell-command-on-region (point-min) (point-max) result t 'no-mark (generate-new-buffer "*Format Errors*")))
      (t ;; The function has already done the conversion
       )))
  (goto-char (point-min)))

(defun nagy-formats-do-convert (into-mode)
  (interactive "SDo Convert: ")
  ;; TODO region-active-p
  ;; TODO set revert-buffer function
  ;; TODO evil mode g- prefix
  ;; TODO embark integration ?
  (let ((oldbuf (current-buffer))
        (newbuf (generate-new-buffer (format "Into-%s" into-mode))))
    (copy-to-buffer newbuf (point-min) (point-max))
    (add-hook 'after-change-functions
              (lambda (&rest _args)
                (when (buffer-live-p newbuf)
                  (with-current-buffer newbuf (erase-buffer))
                  (copy-to-buffer newbuf (point-min) (point-max))
                  (with-current-buffer newbuf
                    (nagy-formats--call-converter (buffer-local-value 'major-mode oldbuf) into-mode)
                    (cl-loop for f in after-change-functions
                             do
                             (when (functionp f)
                               (funcall f (point-min) (point-max) (point-max)))))))
              nil t)
    (with-current-buffer newbuf
      (nagy-formats--call-converter (buffer-local-value 'major-mode oldbuf) into-mode)
      (setq-local nagy-formats--base-buffer oldbuf)
      (set-window-buffer nil (current-buffer)))))
(keymap-global-set "H-M-b" #'nagy-formats-do-convert)

(cl-defgeneric nagy-formats-convert (from to)
  "My documentation."
  (message "converting from %S to %S" from to ))

(add-to-list 'format-alist '(json2yaml "json2yaml" nil ",json,yaml" nil nil nil))
(cl-defmethod nagy-formats-convert ((from (eql js-json-mode)) (to (eql yaml-mode)))
  (ignore from to )
  (format-decode-buffer 'json2yaml)
  (unless (eq major-mode 'yaml-mode)
    (yaml-mode)))

;; (add-to-list 'format-alist '(any2toml "any2toml" nil "yj -jt" nil nil nil))
(cl-defmethod nagy-formats-convert ((from (eql js-json-mode)) (to (eql conf-toml-mode)))
  (ignore from to )
  ;; (format-decode-buffer 'any2toml)
  (shell-command-on-region (point-min) (point-max) "yj -jt -i" t 'no-mark (generate-new-buffer "*Format Errors*"))
  (unless (eq major-mode 'conf-toml-mode)
    (conf-toml-mode)))

(cl-defmethod nagy-formats-convert (from (to (eql b64)))
  (ignore from to )
  "base64")

(cl-defmethod nagy-formats-convert (from (to (eql hex)))
  (ignore from to )
  ",any,hex")

(cl-defmethod nagy-formats-convert ((from (eql yaml-mode)) (to (eql js-json-mode)))
  (ignore from to )
  (shell-command-on-region (point-min) (point-max) "yj -yj -i" t 'no-mark (generate-new-buffer "*Format Errors*"))
  (unless (eq major-mode 'js-json-mode)
    (js-json-mode)))

(defun mold-or-call-text-mode ()
  "Mold Or Call the text-mode"
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (let ((filename (car (dired-get-marked-files t))))
      (cond
       ((string-suffix-p ".pcap" filename)
        (with-current-buffer (generate-new-buffer (format "Into-text-mode: %s" filename))
          (insert-file-contents-literally filename)
          (format-decode-buffer 'pcap2txt)
          (text-mode)
          (set-window-buffer nil (current-buffer))))
       (t (user-error "No Conversion to text-mode found.")))))
   (t (text-mode))))
(keymap-global-set "H-M-t" #'mold-or-call-text-mode)

(provide 'nagy-formats)
;;; nagy-formats.el ends here
