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
;; Package-Requires: ((emacs "29.1") evil eat evil-numbers evil-surround evil-goggles general nagy-use-package)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'nagy-use-package)

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
  ;; Does not work because deletion commands also are affected
  ;; (evil-respect-visual-line-mode t)
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
  :preface
  (require 'esh-mode)
  (defun nagy-eshell-clear-scrollback ()
    (interactive)
    (eshell/clear-scrollback)
    (eshell-send-input))
  :bind
  ("<s-return>" . eshell)
  (:map eshell-mode-map
        ("H-h" . delete-backward-char)
        ("H-ö" . eshell-previous-input)
        ([remap revert-buffer-quick] . nagy-eshell-clear-scrollback)
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

(use-package evil-surround
  :bind
  ("H-(" . evil-surround-region)
  ("H-)" . evil-surround-delete))

(use-package shell
  :config
  (evil-set-initial-state 'shell-mode 'normal))

(use-package sqlite-mode
  ;; Tracking issue https://github.com/emacs-evil/evil-collection/issues/749
  :general
  (:states 'normal :keymaps 'sqlite-mode-map
           "H-r" #'sqlite-mode-list-tables
           "f" #'sqlite-mode-list-data
           "RET" #'sqlite-mode-list-data)
  :same "^*SQLite ")

(use-package sql
  :abbrev 'sql-mode
  ("c" . "create")
  ("t" . "table")
  ("s" . "select")
  ("i" . "insert")
  ("u" . "update")
  ("f" . "from")
  ("d" . "delete")
  ("dr" . "drop")
  ("e" . "exists"))

;; (use-package evil-collection-calc
;;   :config
;;   (evil-set-initial-state 'calc-mode 'emacs))

(use-package eat
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-directory-tracking nil)
  :config
  (evil-set-initial-state 'eat-mode 'emacs)
  :bind
  ("<key-chord> ü x" . eat))

(use-package tar-mode
  :general
  (:states 'normal :keymaps 'tar-mode-map
           "f" #'tar-extract))

(require 'evil-goggles)
(use-package evil-goggles
  :diminish 'evil-goggles-mode
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  :config
  (evil-goggles-mode 1))

(provide 'nagy-evil)
;;; nagy-evil.el ends here
