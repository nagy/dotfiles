;;; nagy-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: December 01, 2022
;; Modified: December 01, 2022
;; Version: 0.0.1
;; Keywords: extensions
;; Homepage: https://github.com/nagy/nagy-evil
;; Package-Requires: ((emacs "29.1") evil evil-numbers general)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)
(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'eshell)
  (require 'esh-mode)
  (require 'evil-numbers))

(use-package evil
  :bind
  ("H-z" . evil-scroll-line-to-center))

(use-package evil-numbers
  :bind
  ("H-<up>" . evil-numbers/inc-at-pt-incremental)
  ("H-<down>" . evil-numbers/dec-at-pt-incremental)
  :general
  (:states 'normal
           "g+" #'evil-numbers/inc-at-pt-incremental
           "g-" #'evil-numbers/dec-at-pt-incremental))

(use-package eshell
  :bind
  ("<s-return>" . eshell)
  (:map eshell-mode-map
        ("H-Ö" . eshell-previous-input)
        ("H-ö" . eshell-send-input)
        ("s-ö" . eshell-send-input)
        ("M-ö" . eshell-send-input))
  :custom
  (eshell-banner-message "")
  (eshell-scroll-to-bottom-on-output nil)
  :general
  (:states 'normal :keymaps 'eshell-mode-map
           "k" #'evil-previous-visual-line
           "j" #'evil-next-visual-line
           "ö" #'eshell-send-input
           "Ö" #'eshell-previous-input))

(provide 'nagy-evil)
;;; nagy-evil.el ends here
