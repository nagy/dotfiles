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

(require 'doom-lib)
(require 'modus-themes)

(defun dayp ()
  "Return non-nil if it is day (aka light theme)."
  (eq (modus-themes--current-theme) 'modus-operandi))

(use-package modus-themes
  :functions +modus-themes-fix-pitches
  :bind
  ("<H-f1>" . doom-big-font-mode)
  ("<H-f2>" . modus-themes-toggle)
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-slanted-constructs t   ; kursiv
        modus-themes-org-blocks 'tinted-background
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-completions '((matches . (background))
                                   (selection . (semibold accented intense))
                                   (popup . (accented))))
  :config
  (defadvice! +modus-themes-fix-pitches (&rest _args)
    :after '(modus-themes-toggle doom-big-font-mode)
    (set-face-attribute 'variable-pitch nil :height 'unspecified)
    (set-face-attribute 'fixed-pitch nil :height 'unspecified)
    (set-face-attribute 'header-line nil :inherit 'unspecified)))



;; ;;;;; telephone-line
;;     `(telephone-line-accent-active ((,class :background ,fg-inactive :foreground ,bg-inactive)))
;;     `(telephone-line-accent-inactive ((,class :background ,bg-active :foreground ,fg-active)))
;;     `(telephone-line-error ((,class :inherit bold :foreground ,red-active)))
;;     `(telephone-line-evil ((,class :foreground ,fg-main)))
;;     `(telephone-line-evil-emacs ((,class :inherit telephone-line-evil :background ,magenta-intense-bg)))
;;     `(telephone-line-evil-insert ((,class :inherit telephone-line-evil :background ,green-intense-bg)))
;;     `(telephone-line-evil-motion ((,class :inherit telephone-line-evil :background ,yellow-intense-bg)))
;;     `(telephone-line-evil-normal ((,class :inherit telephone-line-evil :background ,bg-alt)))
;;     `(telephone-line-evil-operator ((,class :inherit telephone-line-evil :background ,yellow-subtle-bg)))
;;     `(telephone-line-evil-replace ((,class :inherit telephone-line-evil :background ,red-intense-bg)))
;;     `(telephone-line-evil-visual ((,class :inherit telephone-line-evil :background ,cyan-intense-bg)))
;;     `(telephone-line-projectile ((,class :foreground ,cyan-active)))
;;     `(telephone-line-unimportant ((,class :foreground ,fg-inactive)))
;;     `(telephone-line-warning ((,class :inherit bold :foreground ,yellow-active)))

(use-package modus-themes
  :functions nagy/modus-theme-overrides
  :defines doom-leader-map
  :bind
  (:map doom-leader-map
        ("tm" . modus-themes-toggle))
  :config
  (defun nagy/modus-theme-overrides ()
    (interactive)
    (window-divider-mode -1)
    (set-face-attribute 'tab-bar-tab-inactive nil :box nil :background (face-attribute 'tab-bar :background nil t))
    (set-face-attribute 'vertical-border nil :foreground (if (dayp) "white" "black"))
    (after! dired
      (set-face-attribute 'dired-header nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)))
  (add-hook! 'modus-themes-after-load-theme-hook #'nagy/modus-theme-overrides)
  (add-hook! 'doom-first-buffer-hook #'nagy/modus-theme-overrides)
  (add-hook! 'doom-big-font-mode-hook #'nagy/modus-theme-overrides)
  (after! shr
    (set-face-attribute 'shr-h1 nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)
    (set-face-attribute 'shr-h2 nil :font "Et Bembo" :height 1.5 :inherit 'modus-themes-heading-2)
    (set-face-attribute 'shr-h3 nil :font "Et Bembo" :height 1.2 :inherit 'modus-themes-heading-3))
  (after! magit
    (set-face-attribute 'magit-diff-added nil :inherit 'org-block)
    (set-face-attribute 'magit-diff-removed nil :inherit 'org-block)
    (set-face-attribute 'magit-diff-context nil :inherit 'org-block)
    (set-face-attribute 'magit-diff-context-highlight nil :inherit 'org-block))
  ;; (set-face-attribute 'modus-themes-diff-removed nil :inherit 'org-block)
  ;; (set-face-attribute 'modus-themes-diff-added nil :inherit 'org-block)
  ;; (set-face-attribute 'modus-themes-diff-focus-removed nil :inherit 'org-block)
  ;; (set-face-attribute 'modus-themes-diff-focus-added nil :inherit 'org-block)
  )

(use-package paren-face
  :functions nagy/fix-parenface
  :config
  (setq paren-face-regexp "[][(){};,]")
  (defun nagy/fix-parenface ()
    (interactive)
    (set-face-attribute 'parenthesis nil :foreground (if (dayp) "#ccc" "#333")))
  (add-hook! 'modus-themes-after-load-theme-hook #'nagy/fix-parenface)
  (nagy/fix-parenface))

(provide 'nagy-modus-themes)
;;; nagy.el ends here
