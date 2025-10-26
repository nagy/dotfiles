;;; nagy-use-package.el --- Description -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "30.1") dash diminish)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'dash)

(eval-when-compile
  (require 'use-package))
(declare-function use-package-process-keywords "use-package-core")
(declare-function use-package-only-one "use-package-core")

;;;###autoload
(defun use-package-normalize/:same (_name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((stringp arg) arg)
       ((and (consp arg) (eq 'rx (car arg))) (eval arg))
       (t (use-package-error (concat label " :same did not get a string")))))))
;;;###autoload
(defun use-package-handler/:same (name-symbol _keyword rgx rest state)
  (use-package-concat (use-package-process-keywords name-symbol rest state)
                      `((add-to-list 'display-buffer-alist
                                     '(,rgx display-buffer-same-window)))))

;;;###autoload
(with-eval-after-load 'use-package-core
  (add-to-list 'use-package-keywords :same t))

;;; prettify symbols
(defvar nagy-pretty-symbols-default
  '((true . "") (false . "")
    (or . "∨") (and . "∧")
    (throw . "⍜")
    (self . "░")
    (def . "ƒ")
    (let . "↘")
    (import . "⟻")
    (return . "⟼")
    (export . "⟼")
    (defclass . "𝑲")
    (eval . "⩏")
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
    (not . "¬")          ; "¬"
    (in . "∃")
    (list . "⋯")
    (setf . "↓")
    (setq . "↡")
    (try . "〜") (except . "☇")
    (if . "꜏") (else . "꜊") (then . "∴")
    (when . "〉") (unless . "〈")
    (loop . "↻")))
(defun nagy-pretty-init (symbol)
  "Set pretty symbols for mode SYMBOL."
  (interactive "SSymbol: ")
  (dolist (i (get symbol 'nagy-pretty))
    (setf (cdr   i) (if (symbolp (cdr i))
                        (alist-get (cdr i) nagy-pretty-symbols-default)
                      (cdr i)))
    (push i prettify-symbols-alist)))

;;;###autoload
(defalias 'use-package-normalize/:pretty 'use-package-normalize-forms)

;;;###autoload
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

;;;###autoload
(with-eval-after-load 'use-package-core
  (add-to-list 'use-package-keywords :pretty t))
;;; abbrev

;;;###autoload
(defalias 'use-package-normalize/:abbrev 'use-package-normalize-forms)

;;;###autoload
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
;;;###autoload
(with-eval-after-load 'use-package-core
  (add-to-list 'use-package-keywords :abbrev t))
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

;;;###autoload
(defalias 'use-package-normalize/:cycle 'use-package-normalize-forms)

;;;###autoload
(defun use-package-handler/:cycle (name-symbol _keyword rgx rest state)
  (use-package-concat
   (use-package-process-keywords name-symbol rest state)
   (cl-loop for x in (cdr rgx)
            for rotated = (cons (car (last x)) (butlast x))
            append
            (cl-loop for zip in (-zip-pair x rotated)
                     collect
                     `(add-to-list 'nagy-cycle-alist ',zip)))))
(keymap-global-set "H-s-f" #'nagy-cycle-dwim)

;;;###autoload
(with-eval-after-load 'use-package-core
  (add-to-list 'use-package-keywords :cycle t)
  (add-to-list 'use-package-merge-key-alist
               '(:pretty . (lambda (new old) (append new old))) t))

;; also try `switch-to-buffer-obey-display-actions'. Does not solve it completely.
(defun nagy-replace-switch-to-buffer-other-window (orig-fun &rest args)
  "Advice that replaces calls to `switch-to-buffer-other-window'
with `switch-to-buffer'."
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (apply orig-fun args)))

;;;###autoload
(pcase-defmacro suffix (suffix)
  "Pattern (suffix SUFFIX) matches if the string ends with SUFFIX."
  `(and (pred stringp)
        (pred (string-suffix-p ,suffix))))

;;;###autoload
(pcase-defmacro prefix (prefix)
  "Pattern (prefix PREFIX) matches if the string starts with PREFIX."
  `(and (pred stringp)
        (pred (string-prefix-p ,prefix))))

;;;###autoload
(pcase-defmacro infix (infix)
  "Pattern (infix INFIX) matches if the string contains INFIX."
  `(and (pred stringp)
        (pred (s-contains-p ,infix))))

;;;###autoload
(pcase-defmacro derived (&rest modes)
  "Pattern (derived MODES...)."
  `(pred (lambda (mode-or-buffer)
           (cl-typecase mode-or-buffer
             (buffer (provided-mode-derived-p
                      (buffer-local-value 'major-mode mode-or-buffer) ,@modes))
             (string (provided-mode-derived-p
                      (buffer-local-value 'major-mode (get-buffer mode-or-buffer)) ,@modes))
             (symbol (provided-mode-derived-p mode-or-buffer ,@modes))))))

(provide 'nagy-use-package)
;;; nagy-use-package.el ends here
