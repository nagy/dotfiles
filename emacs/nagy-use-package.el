;;; nagy-use-package.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: February 11, 2023
;; Modified: February 11, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy-use-package
;; Package-Requires: ((emacs "29.1") ov)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'ov)
(require 'use-package)

(defun use-package-normalize/:same (_name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((stringp arg) arg)
       (t (use-package-error (concat label " :same did not get a string")))))))
(defun use-package-handler/:same (name-symbol _keyword rgx rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (if (null rgx)
        body
      (use-package-concat
       body
       `((push '(,rgx display-buffer-same-window) display-buffer-alist))))))
(add-to-list 'use-package-keywords :same t)

;;; prettify symbols
(defvar nagy-pretty-symbols-default
  '((true . "✔") (false . "✘")
    (throw . "⍜")
    (self . "░")
    (def . "ƒ")
    (let . "↘")
    (import . "⟻") (return . "⟼")
    (defclass . "𝕂")
    (object . "𝕆")
    (map . "ℍ")
    (hash . "♯")
    (meta . "▽")
    (tostring . "𝕊")
    (source . "♁")
    (stdlib . "○")
    (null . "∅")
    (new . "↙")
    (print . "⚶")
    (const . "𝕔")
    (try . "〜") (except . "☇")
    (if . "꜏") (else . "꜊") (then . "∴")
    (loop . "↻")))
(defun nagy-pretty-init (symbol)
  "Set pretty symbols."
  (interactive "SSymbol: ")
  (dolist (i (get symbol 'nagy-pretty))
    (setf (cdr   i) (if (symbolp (cdr i))
                        (alist-get (cdr i) nagy-pretty-symbols-default)
                      (cdr i)))
    (if (= 1 (length (cdr i)))
        (push i prettify-symbols-alist)
      (ov-set (car i) 'display (cdr i)))))
(defalias 'use-package-normalize/:pretty 'use-package-normalize-forms)
(defun use-package-handler/:pretty (name-symbol _keyword rgx rest state)
  (let* ((body (use-package-process-keywords name-symbol rest state))
         (mode (car rgx))
         (mode-name (symbol-name (cadr mode))))
    (if (null rgx)
        body
      (use-package-concat
       body
       `((declare-function ,(intern (concat "nagy-pretty-" mode-name "-h"))"emacs" ()))
       `((defun ,(intern (concat "nagy-pretty-" mode-name "-h")) ()
           ,(format "Set pretty symbols for `%s'. Auto generated." mode-name)
           (nagy-pretty-init ,mode)))
       `((add-hook ',(intern (concat mode-name "-hook")) #',(intern (concat "nagy-pretty-" mode-name "-h"))))
       `((setf (get ,mode 'nagy-pretty) ',(cdr rgx)))))))
(add-to-list 'use-package-keywords :pretty t)

(provide 'nagy-use-package)
;;; nagy-use-package.el ends here
