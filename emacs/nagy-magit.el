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
;; Package-Requires: ((emacs "29.1") magit-section forge general with-editor)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)

(use-package magit
  ;; :preface
  ;; (defun nagy-magit-branch-checkout-at-point ()
  ;;   (interactive)
  ;;   (magit-branch-checkout (magit-branch-at-point)))
  :custom
  (magit-pull-or-fetch t)
  :config
  (push '("^magit-revision" display-buffer-same-window) display-buffer-alist)
  (push '("^magit-stash" display-buffer-same-window) display-buffer-alist)
  (push '("^magit:" display-buffer-same-window) display-buffer-alist)
  (push '("^magit-diff:"  display-buffer-same-window) display-buffer-alist)
  :bind
  ("H-g" . magit-status)
  (:map magit-log-select-mode-map
        ([remap save-kill-buffer] . magit-log-select-pick)
        )
  (:map magit-mode-map
        ;; ("H-." . nagy-magit-branch-checkout-at-point)
        ;; ("H-_" . magit-rebase-onto-upstream)
        )
  (:map magit-diff-mode-map
        ("SPC" . nil)          ;; was `scroll-up'
        ;; ("H-_" . magit-rebase-onto-upstream)
        )
  )

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
        ("H-a" . magit-section-cycle)
        ("C-ö" . magit-section-cycle-global)
        ("H-j" . magit-section-forward)
        ("H-k" . magit-section-backward)))

(use-package forge
  :bind
  ("H-ß" . forge-dispatch)
  (:map forge-post-mode-map
        ([remap save-kill-buffer] . forge-post-submit)
        ([remap kill-this-buffer] . forge-post-cancel)
        )
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
