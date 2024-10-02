;;; nagy-misc.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: March 03, 2023
;; Modified: March 03, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy-misc
;; Package-Requires: ((emacs "29.1") nameless golden-ratio macrostep ts ov paren-face systemd tokei wgrep focus eros git-modes osm literate-calc-mode nhexl-mode breadcrumb sotlisp general nagy-use-package)
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

(use-package emacs
  :general
  (:states 'normal
           "²" #'duplicate-dwim))

(use-package nameless
  :diminish nameless-mode
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
     ("□" . "blocker")
     ("▱" . "map")                      ; or 𝒎
     ("ℕd" . "nagy-data")
     ("ℕ" . "nagy")
     ("⧖" . "dired")
     ("ol" . "org-link")
     ("ox" . "org-export")
     ("ob" . "org-babel")
     ("o" . "org")
     ("ŧ" . "tokei")
     ("√" . "calc")
     ("e4" . "elforth"))))

;; TODO replace with golden mode
(use-package golden-ratio
  :bind
  ("H-s-=" . golden-ratio-mode))

(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
        ("<normal-state> µ" . macrostep-expand))
  (:map macrostep-mode-map
        ("c" . always)
        ("s-k" . macrostep-collapse-all)))

(use-package eww
  :bind
  ("s-€" . eww)
  :hook
  (eww-mode . variable-pitch-mode))

(defun nagy/delete-paragraph ()
  (interactive)
  (if (region-active-p)
      (progn
        (copy-region-as-kill (region-beginning) (region-end))
        (delete-region (region-beginning) (region-end)))
    (save-excursion
      (mark-paragraph)
      (copy-region-as-kill (region-beginning) (region-end))
      (delete-region (region-beginning) (region-end)))))
(defun nagy/yank-paragraph ()
  (interactive)
  (if (region-active-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (save-mark-and-excursion
      (mark-paragraph)
      (copy-region-as-kill (region-beginning) (region-end)))))

(use-package prog-mode
  :hook
  (prog-mode . visual-line-mode))

(use-package tokei
  :bind
  ("M-⧖" . tokei))

(use-package wgrep
  :bind
  (:map wgrep-mode-map
        ([remap kill-this-buffer] . wgrep-abort-changes)
        ([remap save-kill-buffer] . wgrep-finish-edit))
  :general
  (:states 'normal :keymaps 'wgrep-mode-map
           "ö" #'wgrep-finish-edit))

(use-package focus
  :preface
  (defun nagy/fix-focus-face ()
    (set-face-attribute 'focus-unfocused nil :foreground 'unspecified :inherit 'parenthesis))
  :init
  (setq focus-mode-to-thing
        '((prog-mode . defun)
          (markdown-mode . paragraph)
          (elpher-mode . paragraph)
          (nix-mode . paragraph)
          (text-mode . sentence)))
  :commands focus-mode
  :bind
  ("H-M-f" . focus-mode)
  :config
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/fix-focus-face))

(use-package eros
  :commands (eros-mode)
  :custom
  (eros-eval-result-prefix "")
  :config
  (eros-mode 1))

(use-package cc-mode
  :defer t
  :pretty 'c-mode
  ("if" . if) ("else" . else) ("endif" . else)
  ("#define" . "»")
  ("#undef" . "«")
  ("void" . null)
  ("return" . return)
  ("while" . loop)
  ("const" . const))

(use-package calc
  :general
  (:states 'normal :keymaps 'calc-edit-mode-map
           "ö" #'calc-edit-finish)
  (:states 'normal :keymaps 'calc-mode-map
           "π" #'calc-pi))

(use-package wdired
  :general
  (:states 'normal :keymaps 'wdired-mode-map
           "ö" #'wdired-finish-edit))

(use-package image
  :general
  (:states 'normal :keymaps 'image-mode-map
           "P" #'image-transform-fit-to-window))

(use-package apropos
  :bind
  (:map apropos-mode-map
        ("H-j" . apropos-next-symbol)
        ("H-k" . apropos-previous-symbol))
  :general
  (:states 'normal :keymaps 'apropos-mode-map
           "f" #'apropos-follow))

(use-package gitconfig-mode
  :defer t
  :pretty 'gitconfig-mode
  ("true" . true) ("false" . false)
  ("branch" . "⌥"))

(use-package info
  :bind
  (:map Info-mode-map
        ("H-j" . Info-next)
        ("H-k" . Info-prev))
  :general
  (:states 'normal :keymaps 'Info-mode-map
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
  :custom
  (custom-buffer-verbose-help nil)
  (custom-search-field nil)
  :general
  (:states 'normal :keymaps 'custom-mode-map
           "f" #'Custom-newline
           "u" #'Custom-goto-parent))

(use-package comint
  ;; :preface
  ;; (defun +nagy/comint-delete-clear ()
  ;;   (interactive)
  ;;   (comint-delete-input)
  ;;   (comint-clear-buffer))
  :hook
  (comint-mode . visual-line-mode)
  :custom
  ;; can freeze emacs on something like sleep 10 if non-nil
  ;; https://old.reddit.com/r/emacs/comments/14377k9/weekly_tips_tricks_c_thread/jn8igpu/
  (comint-process-echoes nil)
  :bind
  (:map comint-mode-map
        ([remap revert-buffer-quick] . comint-clear-buffer)
        ("<key-chord> f j" . comint-send-input)
        ("H-Ö" . comint-previous-input)
        ("M-Ö" . comint-previous-input)
        ("H-d" . comint-send-eof)
        ("H-j" . comint-next-prompt)
        ("H-k" . comint-previous-prompt)))

(use-package sotlisp
  :diminish 'sotlisp-mode)

(use-package gitattributes-mode
  ;; also catch files in nix store
  :mode "-gitattributes\\'")

(use-package gitconfig-mode
  ;; also catch files in nix store
  :mode "-gitconfig\\'")

;; (use-package gitignore-mode
;;   ;; also catch files in nix store
;;   :mode "-gitignore\\'")

(require 'hideshow)
(use-package hideshow
  :preface
  (defun nagy-hs-toggle-hiding ()
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding)))
  :general
  (:states 'normal
           "r" #'nagy-hs-toggle-hiding))

(use-package ielm
  :general
  (:states 'normal :keymaps 'inferior-emacs-lisp-mode-map
           "ö" #'ielm-return))

(use-package osm
  :custom
  (osm-copyright nil)
  :bind
  (:map osm-mode-map
        ("<home>" . osm-left-left)
        ("<end>" . osm-right-right)
        ("<next>" . osm-down-down)
        ("<prior>" . osm-up-up))
  :same "^\\*osm")

(use-package literate-calc-mode
  :defer t
  :custom
  (literate-calc-mode-idle-time .1)
  ;; (literate-calc-mode-idle-time nil)
  )

(use-package breadcrumb
  :bind
  ("C-H-j" . breadcrumb-jump)
  :general
  (:states 'normal
           "¿" #'breadcrumb-mode))

(use-package tabulated-list
  :general
  (:states 'normal :keymaps 'tabulated-list-mode-map
           "ö" #'tabulated-list-widen-current-column
           "a" #'tabulated-list-narrow-current-column
           "s" #'tabulated-list-sort
           "h" #'tabulated-list-previous-column
           "l" #'tabulated-list-next-column))

(use-package nhexl-mode
  :general
  (:states 'normal
           "⬡" #'nhexl-mode))

(provide 'nagy-misc)
;;; nagy-misc.el ends here
