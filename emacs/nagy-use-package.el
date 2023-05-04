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
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

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

(provide 'nagy-use-package)
;;; nagy-use-package.el ends here
