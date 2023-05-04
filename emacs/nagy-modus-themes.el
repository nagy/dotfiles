;;; nagy-modus-themes.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: October 06, 2022
;; Modified: October 06, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: (paren-face modus-themes)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'modus-themes)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'paren-face))

(defun dayp ()
  "Return non-nil if it is day (aka light theme)."
  (eq (modus-themes--current-theme) 'modus-operandi))

(use-package modus-themes
  :functions +modus-themes-fix-pitches
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-slanted-constructs t   ; kursiv
        modus-themes-org-blocks 'tinted-background
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-completions '((matches . (background))
                                   (selection . (semibold accented intense))
                                   (popup . (accented))))
  ;; :config
  ;; (defadvice! +modus-themes-fix-pitches (&rest _args)
  ;;   :after '(modus-themes-toggle doom-big-font-mode)
  ;;   (set-face-attribute 'variable-pitch nil :height 'unspecified)
  ;;   (set-face-attribute 'fixed-pitch nil :height 'unspecified)
  ;;   (set-face-attribute 'header-line nil :inherit 'unspecified))
  )

(use-package modus-themes
  :functions nagy/modus-theme-overrides
  :config
  (defun nagy/modus-theme-overrides ()
    (interactive)
    (window-divider-mode -1)
    (set-face-attribute 'tab-bar-tab-inactive nil :box nil :background (face-attribute 'tab-bar :background nil t))
    (set-face-attribute 'vertical-border nil :foreground (if (dayp) "white" "black"))
    (with-eval-after-load 'dired
      (set-face-attribute 'dired-header nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)))
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/modus-theme-overrides)
  (add-hook 'doom-first-buffer-hook #'nagy/modus-theme-overrides)
  (add-hook 'doom-big-font-mode-hook #'nagy/modus-theme-overrides)
  (with-eval-after-load 'shr
    (set-face-attribute 'shr-h1 nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)
    (set-face-attribute 'shr-h2 nil :font "Et Bembo" :height 1.5 :inherit 'modus-themes-heading-2)
    (set-face-attribute 'shr-h3 nil :font "Et Bembo" :height 1.2 :inherit 'modus-themes-heading-3))
  (with-eval-after-load 'magit
    (set-face-attribute 'magit-diff-added nil :inherit 'org-block)
    (set-face-attribute 'magit-diff-removed nil :inherit 'org-block)
    (set-face-attribute 'magit-diff-context nil :inherit 'org-block)
    (set-face-attribute 'magit-diff-context-highlight nil :inherit 'org-block)))

(use-package paren-face
  :functions nagy/fix-parenface
  :config
  (setq paren-face-regexp "[][(){};,]")
  (defun nagy/fix-parenface ()
    (interactive)
    (set-face-attribute 'parenthesis nil :foreground (if (dayp) "#ccc" "#333")))
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/fix-parenface)
  (nagy/fix-parenface))

(provide 'nagy-modus-themes)
;;; nagy.el ends here
