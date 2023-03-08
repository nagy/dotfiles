;;; nagy-emacs.el --- config emacs packages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: March 03, 2023
;; Modified: March 03, 2023
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/nagy/nagy-emacs
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Configuration of emacs internal packages
;;
;;; Code:

(use-package help
  :hook
  (help-mode . visual-fill-column-mode)
  :bind
  ("C-H-h" . describe-key-briefly))

(use-package repeat
  :bind
  ("M-s-." . repeat))

(use-package man
  :custom
  (Man-notify-method 'pushy))

(use-package compile
  :bind
  ("C-M-¢" . compile)
  ("H-s-j" . next-error)
  ("H-s-k" . previous-error)
  (:map compilation-minor-mode-map
        ([remap revert-buffer-quick] . recompile)
        ("H-j" . compilation-next-error)
        ("H-k" . compilation-previous-error)))

(use-package tabulated-list
  ;; :custom
  ;; (text-scale-remap-header-line t)
  :bind
  (:map tabulated-list-mode-map
        ("M-s M-s" . tabulated-list-sort)))

(use-package shr
  :defer t
  :config
  (setq shr-inhibit-images t)
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil))

(use-package files
  :bind
  ("s-f" . find-file))

(use-package calc
  :config
  ;; https://old.reddit.com/r/emacs/comments/hujbbm/why_calceval_390010015_026_instead_585/
  (setq calc-multiplication-has-precedence nil)
  :bind
  ("<XF86Calculator>" . calc-dispatch)
  ("S-<XF86Calculator>" . calc-embedded-update-formula))

(use-package tab-bar
  :config
  (tab-bar-mode)
  :custom
  (tab-bar-show 1)
  (tab-bar-auto-width t)
  (tab-bar-auto-width-max '(440 40))
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice t))

(provide 'nagy-emacs)
;;; nagy-emacs.el ends here
