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
;; Package-Requires: ((emacs "29.1") ov dash)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'ov)
(require 'dash)
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
    (assert . "𝒂")
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
;;; abbrev
(defalias 'use-package-normalize/:abbrev 'use-package-normalize-forms)
(defun use-package-handler/:abbrev (name-symbol _keyword rgx rest state)
  (let* ((body (use-package-process-keywords name-symbol rest state))
         (mode (car rgx))
         (mode-name (symbol-name (cadr mode))))
    (if (null rgx)
        body
      (use-package-concat
       body
       (cl-loop for x in (cdr rgx)
                collect
                `(define-abbrev ,(intern (concat mode-name "-abbrev-table")) ,(car x) ,(cdr x) nil :system t))
       ))))
(add-to-list 'use-package-keywords :abbrev t)
;;; cycle
;; TODO look at https://melpa.org/#/cycle-at-point
(defvar nagy-cycle-alist nil)
(defun nagy-cycle-dwim ()
  (interactive)
  (-let* (((beg . end) (bounds-of-thing-at-point 'symbol))
          (sym (buffer-substring-no-properties beg end)))
    (when (member sym (cl-loop for cell in nagy-cycle-alist
                               collect (car cell)))
      (delete-region beg end)
      (insert (alist-get sym nagy-cycle-alist "" nil 'equal)))))
(defalias 'use-package-normalize/:cycle 'use-package-normalize-forms)
(defun use-package-handler/:cycle (name-symbol _keyword rgx rest state)
  (use-package-concat
   (use-package-process-keywords name-symbol rest state)
   (cl-loop for x in (cdr rgx)
            for rotated = (cons (car (last x)) (butlast x))
            append
            (cl-loop for zip in (-zip-pair x rotated)
                     collect
                     `(push ',zip nagy-cycle-alist)))))
(add-to-list 'use-package-keywords :cycle t)

(provide 'nagy-use-package)
;;; nagy-use-package.el ends here
