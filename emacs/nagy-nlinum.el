;;; nagy-nlinum.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: January 27, 2023
;; Modified: January 27, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy-nlinum
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package display-line-numbers
  ;; :custom
  ;; (display-line-numbers-type t)
  :bind
  ("M-ĸ" . display-line-numbers-mode)
  ("s-M-ĸ" . global-display-line-numbers-mode))

(provide 'nagy-nlinum)
;;; nagy-nlinum.el ends here
