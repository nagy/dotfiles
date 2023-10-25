;;; nagy-modus-themes.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
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
;; Package-Requires: ((emacs "29.1") paren-face modus-themes ef-themes lin)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'modus-themes)
(require 'ef-themes)
(require 'paren-face)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'lin))

(use-package modus-themes
  :preface
  (defun dayp ()
    "Return non-nil if it is day (aka light theme)."
    (eq (modus-themes--current-theme) 'modus-operandi))
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)   ; kursiv
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-completions
   '((matches . (background))
     (selection . (semibold accented intense))
     (popup . (accented))))
  (modus-themes-common-palette-overrides
   '((bg-mode-line-active bg-main)
     (bg-mode-line-inactive bg-inactive)
     (bg-region bg-blue-subtle)
     (fg-region unspecified)
     ;; (bg-tab-bar bg-main)
     ;; (bg-tab-current bg-active)
     ;; (bg-tab-other bg-inactive)
     )))

(use-package modus-themes
  :functions nagy/modus-theme-overrides
  :config
  (defun nagy/modus-theme-overrides ()
    (interactive)
    (set-face-attribute 'tab-bar-tab-inactive nil :box nil :background (face-attribute 'tab-bar :background nil t))
    (set-face-attribute 'vertical-border nil :foreground (if (dayp) "white" "black"))
    (with-eval-after-load 'dired
      (set-face-attribute 'dired-header nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)))
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/modus-theme-overrides)
  (add-hook 'doom-first-buffer-hook #'nagy/modus-theme-overrides)
  (add-hook 'doom-big-font-mode-hook #'nagy/modus-theme-overrides))

(use-package paren-face
  :functions nagy/fix-parenface
  :config
  (setq paren-face-regexp "[][(){};,]")
  (defun nagy/fix-parenface ()
    (interactive)
    (set-face-attribute 'parenthesis nil :foreground (if (dayp) "#ccc" "#333")))
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/fix-parenface)
  (nagy/fix-parenface))

(use-package lin
  ;; :preface
  ;; (require 'lin)
  :functions lin-global-mode
  :config
  (setq lin-mode-hooks
        '(dired-mode-hook
          elfeed-search-mode-hook
          grep-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          nix-search-mode-hook
          proced-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode 1))


(provide 'nagy-modus-themes)
;;; nagy-modus-themes.el ends here
