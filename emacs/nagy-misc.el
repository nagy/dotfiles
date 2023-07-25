;;; nagy-misc.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: March 03, 2023
;; Modified: March 03, 2023
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/nagy/nagy-misc
;; Package-Requires: ((emacs "29.1") nameless golden-ratio macrostep ts ov paren-face systemd tokei wgrep focus eros git-modes general nagy-use-package)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'ov)
(require 'general)

(require 'nagy-use-package)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'nameless))

(use-package nameless
  :diminish nameless-mode
  :hook
  (emacs-lisp-mode . nameless-mode)
  :general
  (:states 'insert :keymaps 'nameless-mode-map
           "s--" #'nameless-insert-name)
  :custom
  (nameless-prefix "─")
  (nameless-private-prefix t)
  (nameless-global-aliases
   '(("fl" . "font-lock")
     ("ms" . "magit-section")
     ("○" . "nix")
     ("〣" . "triples")
     ("□" . "blocker")
     ("▱" . "map")                      ; or 𝒎
     ("▒" . "nagy")                     ; or ℕ
     ("⧖" . "dired")
     ("ø" . "org")
     ("ŧ" . "tokei")
     ("√" . "calc"))))

(use-package golden-ratio
  :bind
  ("H-s-=" . golden-ratio-mode))

(use-package macrostep
  :bind
  (:map macrostep-mode-map
        ("c" . always)
        ("s-k" . macrostep-collapse-all)))

(use-package eww
  :bind
  ("s-€" . eww)
  :hook
  ;; (eww-mode . visual-fill-column-mode)
  (eww-mode . variable-pitch-mode))

(use-package eros
  :custom
  (eros-eval-result-prefix ""))

(use-package cc-mode
  :pretty 'c-mode
  ("if" . if) ("else" . else)
  ("#define" . "⨠")
  ("const" . const))

;; (use-package literate-calc-mode
;;   :config
;;   (setq literate-calc-mode-idle-time 0.1))

(use-package calc
  :general
  (:states 'normal :keymaps 'calc-edit-mode-map
           "ö" #'calc-edit-finish)
  (:states 'normal :keymaps 'calc-mode-map
           "π" #'calc-pi))

(use-package gitconfig-mode
  :pretty 'gitconfig-mode
  ("true" . true) ("false" . false)
  ;; ("core" . "♁")
  ("branch" . "⌥")
  ;; ("remote" . "🌐")
  )

(use-package info
  :hook
  (Info-mode . visual-fill-column-mode)
  :bind
  (:map Info-mode-map
        ("H-j" . Info-next)
        ("H-k" . Info-prev))
  :general
  (:states 'normal :keymaps 'Info-mode-map
           ;; "o" #'nagy-hint-open-link
           "f" #'Info-follow-nearest-node))

(use-package cus-edit
  :bind
  (:map custom-mode-map
        ([remap revert-buffer-quick] . Custom-reset-saved)
        ([remap save-buffer] . Custom-set)
        ([remap save-kill-buffer] . Custom-buffer-done))
  (:map custom-field-keymap
        ([remap revert-buffer-quick] . Custom-reset-saved)
        ([remap save-buffer] . Custom-set)
        ([remap save-kill-buffer] . Custom-buffer-done))
  :hook
  (Custom-mode . visual-fill-column-mode)
  :custom
  (custom-buffer-verbose-help nil)
  (custom-search-field nil)
  :general
  (:states 'normal :keymaps 'custom-mode-map
           "f" #'Custom-newline
           "u" #'Custom-goto-parent))

(provide 'nagy-misc)
;;; nagy-misc.el ends here
