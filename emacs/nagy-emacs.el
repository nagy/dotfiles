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

(require 'comint)

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

(use-package wdired
  :bind
  (:map wdired-mode-map
        ([remap save-kill-buffer] . wdired-finish-edit)
        ([remap kill-this-buffer] . wdired-abort-changes))
  :custom
  (dired-vc-rename-file nil)) ; without this, some problems occured

(use-package abbrev
  :defines (nix-repl-mode-abbrev-table inferior-emacs-lisp-mode-abbrev-table)
  :diminish abbrev-mode
  :hook
  (prog-mode . abbrev-mode)
  (comint-mode . abbrev-mode)
  (text-mode . abbrev-mode)
  :config
  (define-abbrev text-mode-abbrev-table "aaa" "AAAH AAAH AAAH" nil :system t)
  (define-abbrev global-abbrev-table "afaict" "as far as I can tell" nil :system t)
  (define-abbrev global-abbrev-table "btw" "by the way" nil :system t)
  (define-abbrev global-abbrev-table "wether" "whether" nil :system t)
  (define-abbrev global-abbrev-table "pov" "point of view" nil :system t)
  (with-eval-after-load 'nix-repl
    (define-abbrev nix-repl-mode-abbrev-table "PK" "pkgs" nil :system t)
    (define-abbrev nix-repl-mode-abbrev-table "wpkgs" "with import <nixpkgs> {}; " nil :system t))
  (with-eval-after-load 'ielm
    (define-abbrev inferior-emacs-lisp-mode-abbrev-table "LEN" "(length )" nil :system t)
    (define-abbrev inferior-emacs-lisp-mode-abbrev-table "CAR" "(car )" nil :system t)
    (define-abbrev inferior-emacs-lisp-mode-abbrev-table "CDR" "(cdr )" nil :system t)
    (define-abbrev inferior-emacs-lisp-mode-abbrev-table "SF" "(setf )" nil :system t)
    (define-abbrev inferior-emacs-lisp-mode-abbrev-table "AL" "(alist-get )" nil :system t)
    (define-abbrev inferior-emacs-lisp-mode-abbrev-table "SQ" "(setq )" nil :system t)
    (define-abbrev inferior-emacs-lisp-mode-abbrev-table "LOOP" "(cl-loop for x in '() do )" nil :system t)
    (define-abbrev inferior-emacs-lisp-mode-abbrev-table "KV" "kill-value" nil :system t)))

(use-package hi-lock
  :bind
  ("A-s-H-." . highlight-symbol-at-point))

(use-package emacs
  ;; :custom
  ;; (help-at-pt-display-when-idle t)
  ;; (help-at-pt-timer-delay 0)
  :bind
  ("H-s-," . describe-char)
  ("H-s-." . display-local-help))

(defun nagy-ielm-init-history ()
  (let ((path (expand-file-name "ielm/history" user-emacs-directory)))
    (make-directory (file-name-directory path) t)
    (setq-local comint-input-ring-file-name path))
  (setq-local comint-input-ring-size 10000)
  (setq-local comint-input-ignoredups t)
  (comint-read-input-ring))
(defun nagy-ielm-write-history (&rest _args)
  (with-file-modes #o600
    (comint-write-input-ring)))

(use-package ielm
  :config
  (advice-add 'ielm-send-input :after #'nagy-ielm-write-history)
  :hook
  (ielm-mode . nagy-ielm-init-history))

(provide 'nagy-emacs)
;;; nagy-emacs.el ends here
