;;; nagy-pcap-converter.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: February 19, 2023
;; Modified: February 19, 2023
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/nagy/nagy-pcap-converter
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'format)
(eval-when-compile
  (require 'cl-lib))

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
        (highlight-lines-matching-regexp (rx (or "UDP" "NTP" "DNS")) 'modus-themes-intense-cyan)
        (highlight-lines-matching-regexp "RST" 'modus-themes-intense-red))))
(add-to-list 'auto-mode-alist '("\\.pcap\\'" . pcap-mode))

(provide 'nagy-pcap-converter)
;;; nagy-pcap-converter.el ends here
