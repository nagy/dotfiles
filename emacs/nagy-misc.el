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
;; Package-Requires: ((emacs "29.1") nameless golden-ratio macrostep ts ov paren-face systemd tokei wgrep focus eros git-modes osm literate-calc-mode nhexl-mode breadcrumb sotlisp anaphora general nagy-use-package)
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

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'nameless))

(use-package emacs
  :general
  (:states 'normal
           "²" #'duplicate-dwim))

(use-package nameless
  :diminish nameless-mode
  ;; :bind
  ;; TODO make this emacs mode only
  ;; ("<key-chord> - e" . nameless-mode)
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
      (copy-region-as-kill (region-beginning) (region-end))
      ;; use `lispyville-yank' here to trigger evil-goggles.
      ;; (lispyville-yank (region-beginning) (region-end) 'line )
      )))

(use-package text-mode
  :bind
  (:map text-mode-map
        ("H-ö" . save-buffer)
        ("H-j" . forward-paragraph)
        ("H-k" . backward-paragraph)
        ("H-d" . nagy/delete-paragraph)
        ("H-y" . nagy/yank-paragraph))
  ;; :config
  ;; (with-eval-after-load 'magit
  ;;   (keymap-set text-mode-map
  ;;               "H-l" #'magit-log-buffer-file))
  :general
  (:states 'normal :keymaps 'text-mode-map
           "ö" #'save-buffer))

(use-package prog-mode
  :bind
  (:map prog-mode-map
        ("H-j" . end-of-defun)
        ("H-k" . beginning-of-defun)
        ("H-ö" . save-buffer)
        ("H-d" . nagy/delete-paragraph)
        ;; ("H-l" . magit-log-buffer-file)
        )
  :hook
  (prog-mode . visual-line-mode)
  :general
  (:states 'normal :keymaps 'prog-mode-map
           "ö" #'save-buffer))

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
  :custom
  (image-auto-resize 'fit-window)
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
  ;; (comint-move-point-for-output t)
  :bind
  (:map comint-mode-map
        ([remap revert-buffer-quick] . comint-clear-buffer)
        ("C-ö" . comint-next-input)
        ("H-Ö" . comint-previous-input)
        ("M-Ö" . comint-previous-input)
        ("H-d" . comint-send-eof)
        ("H-_" . comint-send-eof)
        ("H-j" . comint-next-prompt)
        ("H-k" . comint-previous-prompt)
        ;; ("<insert-state> <key-chord> f j" . comint-send-input)
        ("<normal-state> <key-chord> f h" . embark-dwim)
        ("<normal-state> <key-chord> f j" . embark-act)
        )
  :general
  (:states 'normal :keymaps 'comint-mode-map
           "Ö" #'comint-previous-input
           "ö" #'comint-send-input
           "_" #'comint-send-eof))

(use-package sotlisp
  ;; :diminish sotlisp-mode
  :commands (speed-of-thought-mode)
  :config
  (speed-of-thought-mode -1)
  (speed-of-thought-mode 1)
  )

(use-package gitattributes-mode
  ;; also catch files in nix store
  :mode "-gitattributes\\'")

(use-package gitconfig-mode
  ;; also catch files in nix store
  :mode "-gitconfig\\'")

(use-package ielm
  :preface
  (defun ielm-on-buffer ()
    (interactive)
    (let* ((buf (current-buffer))
           (ielm-buf-name (concat "*ielm-" (buffer-name buf) "*")))
      (aif (get-buffer ielm-buf-name)
          (switch-to-buffer it)
        (ielm ielm-buf-name)
        (with-current-buffer ielm-buf-name
          (setq-local ielm-working-buffer buf)))))
  (defun nagy-misc-ielm-hook ()
    (setq mode-line-process '(":" (:eval (buffer-name ielm-working-buffer)))))
  ;; :config
  ;; (advice-add 'ielm-return :after #'evil-normal-state)
  :hook
  (inferior-emacs-lisp-mode . nagy-misc-ielm-hook)
  :general
  (:states 'normal :keymaps 'inferior-emacs-lisp-mode-map
           "ö" #'ielm-return)
  ;; :config
  ;; (keymap-set evil-normal-state-map "<key-chord> ü e" #'ielm-on-buffer)
  )

(use-package osm
  :custom
  (osm-copyright nil)
  (osm-max-age nil)
  :bind
  (:map osm-mode-map
        ("<home>" . osm-left-left)
        ("<end>" . osm-right-right)
        ("<next>" . osm-down-down)
        ("<prior>" . osm-up-up))
  ;; :config
  ;; (require 'osm-ol)
  ;; (put 'osm-bookmark-jump 'bookmark-handler-type "Osm")
  ;; (evil-set-initial-state 'osm-mode 'emacs)
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
  :bind
  (:map tabulated-list-mode-map
        ("M-h" . tabulated-list-previous-column)
        ("M-l" . tabulated-list-next-column))
  :general
  (:states 'normal :keymaps 'tabulated-list-mode-map
           "{" #'tabulated-list-narrow-current-column
           "}" #'tabulated-list-widen-current-column
           "t" #'tabulated-list-sort
           ;; "i" #'tabulated-list-previous-column
           ;; "o" #'tabulated-list-next-column
           ))

(use-package nhexl-mode
  :bind
  ("H-M-H" . nhexl-mode)
  ;; todo: upstream this into evil-collection
  :general
  (:states 'normal :keymaps 'nhexl-mode-map
           [remap evil-next-line] #'nhexl-next-line
           [remap evil-previous-line] #'nhexl-previous-line
           "0" #'nhexl-move-beginning-of-line
           "$" #'nhexl-move-end-of-line
           "w" #'nhexl-nibble-forward
           "b" #'nhexl-nibble-backward
           "gg" #'beginning-of-buffer
           "G" #'end-of-buffer)
  ;; (:states 'normal
  ;;          "⬡" #'nhexl-mode)
  )

;; similar to doom defaults
;; (use-package drag-stuff
;;   :defer t
;;   :init
;;   (map! "<H-up>"    #'drag-stuff-up
;;         "<H-down>"  #'drag-stuff-down
;;         "<H-left>"  #'drag-stuff-left
;;         "<H-right>" #'drag-stuff-right))

;; (use-package macrostep
;;   :config
;;   (map! :n "µ" #'macrostep-expand))

(provide 'nagy-misc)
;;; nagy-misc.el ends here
