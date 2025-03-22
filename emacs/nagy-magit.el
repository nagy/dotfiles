;;; nagy-magit.el --- Description -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "30.1") magit-section forge with-editor)
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
  (add-to-list 'display-buffer-alist '("^magit-revision" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^magit-stash" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^magit-process:" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^magit-diff:"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^magit:" display-buffer-same-window))
  (add-to-list 'Info-url-alist '(("Magit" "Forge") . "https://magit.vc/manual/%m.html#%n"))
  :bind
  ("H-g" . magit-status)
  (:map magit-diff-mode-map
        ("SPC" . nil)          ;; was `scroll-up'
        )
  (:map magit-log-select-mode-map
        ([remap save-kill-buffer] . magit-log-select-pick)
        ([remap nagy-kill-this-buffer] . magit-log-select-quit)
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
        ("H-k" . magit-section-backward)
        ("<normal-state> <key-chord> f h" . embark-dwim)
        ("<normal-state> <key-chord> f j" . embark-act)
        ))

(use-package forge
  :bind
  ("H-ß" . forge-dispatch)
  (:map forge-post-mode-map
        ([remap save-kill-buffer] . forge-post-submit)
        ([remap kill-this-buffer] . forge-post-cancel)
        ([remap nagy-kill-this-buffer] . forge-post-cancel))
  (:map forge-topic-mode-map
        ;; ("M-↓" . forge-pull)
        ("M-w" . forge-copy-url-at-point-as-kill)
        )
  (:map magit-mode-map
        ("M-ß" . forge-pull))
  (:map dired-mode-map
        ("M-ß" . forge-pull))
  :general
  (:states 'normal :keymaps 'magit-mode-map
           "ß" #'forge-dispatch)
  ;; (:states 'normal :keymaps 'forge-post-mode-map
  ;;          "ö" #'forge-post-submit)
  :config
  (setq forge-post-mode-hook (delq 'turn-on-flyspell forge-post-mode-hook))
  (setq forge-post-mode-hook (delq 'visual-line-mode forge-post-mode-hook))
  ;; (set-face-attribute 'forge-pullreq-open nil :inherit 'modus-themes-heading-1)
  ;; remove flyspell and visual-line-mode
  ;; (setq forge-post-mode-hook nil)
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
