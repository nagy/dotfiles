;;; nagy-magit.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: December 01, 2022
;; Modified: December 01, 2022
;; Version: 0.0.1
;; Keywords: extensions
;; Homepage: https://github.com/nagy/nagy-magit
;; Package-Requires: ((emacs "29.1") magit-section forge embark general with-editor)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'bookmark)
(require 'general)
(require 'embark)

(use-package magit
  :bind
  ("H-g" . magit-status)
  :custom
  (magit-pull-or-fetch t))

(use-package magit-section
  :general
  (:states 'normal :keymaps 'magit-section-mode-map
           ;; "r" #'magit-section-toggle  ; already rebase in magit itself
           "Ö" #'magit-section-cycle
           "ö" #'magit-section-toggle)
  :bind
  (:map magit-section-mode-map
        ("s-<kp-1>" . magit-section-show-level-1)
        ("s-<kp-2>" . magit-section-show-level-2)
        ("s-<kp-3>" . magit-section-show-level-3)
        ("s-<kp-4>" . magit-section-show-level-4)
        ("C-ö" . magit-section-cycle-global)
        ("H-j" . magit-section-forward)
        ("H-k" . magit-section-backward)))

(use-package forge
  :bind
  ("H-ß" . forge-dispatch)
  (:map forge-post-mode-map
        ([remap kill-this-buffer] . forge-post-cancel)
        ([remap save-kill-buffer] . forge-post-submit))
  ;; (:map forge-topic-mode-map
  ;;       ("M-↓" . forge-pull))
  (:map magit-mode-map
        ("M-ß" . forge-pull))
  (:map dired-mode-map
        ("M-ß" . forge-pull))
  :general
  (:states 'normal :keymaps 'magit-mode-map
           "ß" #'forge-dispatch)
  ;; (:states 'normal :keymaps 'forge-post-mode-map
  ;;          "ö" #'forge-post-submit)
  )

(use-package with-editor
  :bind
  (:map with-editor-mode-map
        ([remap save-kill-buffer] . with-editor-finish)
        ([remap kill-this-buffer] . with-editor-cancel))
  :general
  (:states 'normal :keymaps 'with-editor-mode-map
           "ö" #'with-editor-finish))

(provide 'nagy-magit)
;;; nagy-magit.el ends here
