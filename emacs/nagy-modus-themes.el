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

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'lin)
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
  (with-eval-after-load 'simple
    (set-face-attribute 'separator-line nil :underline "white"))
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

(use-package lin
  :functions lin-global-mode
  :config
  (setq lin-face 'lin-blue)
  (setq lin-mode-hooks
        '(dired-mode-hook
          elfeed-search-mode-hook
          grep-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          ledger-report-mode-hook
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
