;;; nagy-evil.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
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
;; Package-Requires: ((emacs "29.1") general evil evil-numbers evil-surround)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'evil)
(require 'general)
(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'eshell)
  (require 'esh-mode)
  (require 'evil-numbers))

(use-package evil
  :custom
  (evil-normal-state-cursor '(hbar . 5))
  (evil-insert-state-cursor '(bar . 5))
  (evil-echo-state nil)
  (evil-mode-line-format 'before)
  :bind
  ("H-z" . evil-scroll-line-to-center)
  ("H-u" . evil-undo))

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
        ("H-ö" . eshell-previous-input)
        ("s-ö" . eshell-send-input)
        ("M-ö" . eshell-send-input))
  :custom
  (eshell-banner-message "")
  (eshell-scroll-to-bottom-on-output nil)
  :config
  (evil-set-initial-state 'eshell-mode 'emacs)
  :general
  (:states 'normal :keymaps 'eshell-mode-map
           "k" #'evil-previous-visual-line
           "j" #'evil-next-visual-line
           "ö" #'eshell-send-input
           "Ö" #'eshell-previous-input))

(use-package evil-surround
  :bind
  ("H-(" . evil-surround-region)
  ("H-)" . evil-surround-delete))

(use-package shell
  :config
  (evil-set-initial-state 'shell-mode 'normal))

;; (use-package sqlite-mode
;;   :same "^*SQLite ")

;; (use-package evil-collection-calc
;;   :config
;;   (evil-set-initial-state 'calc-mode 'emacs))

(provide 'nagy-evil)
;;; nagy-evil.el ends here
