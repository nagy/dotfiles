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
;; (require 'hi-lock)

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
  (if (stringp method)
      (let ((error-buff (get-buffer-create "*Format Errors*"))
            (coding-system-for-write 'no-conversion)
            (inhibit-read-only t)
            format-alist)
        (with-current-buffer error-buff
          (widen)
          (erase-buffer))
        (if (and (zerop (save-window-excursion
                          (shell-command-on-region from to method t 'no-mark error-buff)))
                 (zerop (with-current-buffer error-buff (buffer-size))))
            (bury-buffer error-buff)
          (switch-to-buffer-other-window error-buff)
          (error "Format decoding failed"))
        (point))
    (funcall method from to)))


(add-to-list 'format-alist '(pcap2txt "pcap2txt" nil ",pcap,txt" nil nil nil))
(add-to-list 'format-alist '(pcap2json "pcap2json" nil ",pcap,json" nil nil nil))
(add-to-list 'format-alist '(json2yaml "json2yaml" nil ",json,yaml" nil nil nil))
(add-to-list 'format-alist '(json2lines "json2lines" nil ",json,lines" nil nil nil))
(add-to-list 'format-alist '(pcap2xml "pcap2xml" nil ",pcap,xml" nil nil nil))
(add-to-list 'format-alist '(any2hex "any2hex" nil ",any,hex" nil nil nil))
(add-to-list 'format-alist '(wasm "wasm" nil "wasm2wat -" "wat2wasm - --output=-" t nil nil) 'append)

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

;; (define-derived-mode wasm-mode fundamental-mode "WASM"
;;   "hello"
;;   (if (derived-mode-p 'wasm-mode)
;;     (format-decode-buffer '(wasm))))
;; (add-to-list 'auto-mode-alist '("\\.wasm\\'" . wasm-mode))


(defun mold-or-call-text-mode ()
  "Mold Or Call the text-mode"
  (interactive)
  (cond
   ;; TODO region-active-p
   ;; TODO set revert-buffer function
   ;; TODO evil mode g- prefix
   ;; TODO embark integration ?
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

(defun mold-or-call-js-json-mode ()
  "Mold Or Call the js-json-mode"
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (let ((filename (car (dired-get-marked-files t))))
      (cond
       ((string-suffix-p ".pcap" filename)
        (with-current-buffer (generate-new-buffer (format "Into-js-json-mode: %s" filename))
          (insert-file-contents-literally filename)
          (format-decode-buffer 'pcap2json)
          (js-json-mode)
          (set-window-buffer nil (current-buffer)))))))
   (t (js-json-mode))))
(keymap-global-set "H-M-j" #'mold-or-call-js-json-mode)

(defun mold-or-call-xml-mode ()
  "Mold Or Call the xml-mode"
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (let ((filename (car (dired-get-marked-files t))))
      (cond
       ((string-suffix-p ".pcap" filename)
        (with-current-buffer (generate-new-buffer (format "Into-xml-mode: %s" filename))
          (insert-file-contents-literally filename)
          (format-decode-buffer 'pcap2xml)
          (xml-mode)
          (set-window-buffer nil (current-buffer)))))))
   (t (xml-mode))))
(keymap-global-set "H-M-x" #'mold-or-call-xml-mode)

(defun mold-or-call-yaml-mode ()
  "Mold Or Call the yaml-mode"
  (interactive)
  (cond
   ((derived-mode-p 'js-json-mode)
    (let ((_oldbuf (current-buffer))
          (newbuf (generate-new-buffer (format "Into-yaml-mode"))))
      (copy-to-buffer newbuf (point-min) (point-max))
      (with-current-buffer newbuf
        (format-decode-buffer 'json2yaml)
        (yaml-mode)
        (set-buffer-modified-p nil)
        (read-only-mode 1)
        (set-window-buffer nil (current-buffer)))))
   (t (yaml-mode))))
(keymap-global-set "H-M-y" #'mold-or-call-yaml-mode)

(defun mold-lines ()
  (interactive)
  (cond
   ((derived-mode-p 'js-json-mode)
    (let ((_oldbuf (current-buffer))
          (newbuf (generate-new-buffer (format "Into-lines-mode"))))
      (copy-to-buffer newbuf (point-min) (point-max))
      (with-current-buffer newbuf
        (format-decode-buffer 'json2lines)
        (prog-mode)
        (set-window-buffer nil (current-buffer)))))
   (t (user-error "No Conversion to lines-mode found."))))
(keymap-global-set "H-M-≢" #'mold-lines)

(defun mold-any-hex ()
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (let ((filename (car (dired-get-marked-files t))))
      (with-current-buffer (generate-new-buffer (format "Into-hex: %s" filename))
        (insert-file-contents-literally filename)
        (format-decode-buffer 'any2hex)
        (text-mode)
        (set-window-buffer nil (current-buffer)))))
   (t (let ((oldbuf (current-buffer))
            (newbuf (generate-new-buffer (format "Into-hex"))))
        (copy-to-buffer newbuf (point-min) (point-max))
        (with-current-buffer newbuf
          (format-decode-buffer 'any2hex)
          (text-mode)
          (set-window-buffer nil (current-buffer)))))))
(keymap-global-set "H-M-#" #'mold-any-hex)


(provide 'nagy-formats)
;;; nagy-formats.el ends here
