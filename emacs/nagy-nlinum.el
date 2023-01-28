;;; nagy-nlinum.el --- Description -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "24.3") (nlinum))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package nlinum
  :bind
  ("M-ĸ" . nlinum-mode))

(provide 'nagy-nlinum)
;;; nagy-nlinum.el ends here
