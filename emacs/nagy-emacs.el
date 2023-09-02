;;; nagy-emacs.el --- config emacs packages -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
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
;; Package-Requires: ((emacs "29.1") ov)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Configuration of Emacs internal packages
;;
;;; Code:

(require 'comint)

(defun save-kill-buffer ()
  "Save and kill a buffer."
  (interactive)
  (save-buffer)
  (kill-buffer))

(use-package emacs
  ;; :custom
  ;; (help-at-pt-display-when-idle t)
  ;; (help-at-pt-timer-delay 0)
  :custom
  (inhibit-startup-screen t)
  (use-short-answers t)
  (message-log-max t)
  (require-final-newline t)
  (kill-ring-max 250)
  (history-delete-duplicates t)
  (delete-by-moving-to-trash t)
  (large-file-warning-threshold (* 100 1000 1000))
  (duplicate-line-final-position -1)    ; to last line
  ;; (backtrace-on-redisplay-error t)
  (browse-url-default-scheme "https")
  :bind
  ("H-M-t" . text-mode)
  ("H-e" . insert-char)
  ("C-H-e" . emoji-insert)
  ("H-r" . revert-buffer-quick)
  ("H-s-," . describe-char)
  ("H-s-." . display-local-help)
  ("H-s-:" . duplicate-dwim))

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
  :custom
  (shr-inhibit-images t)
  (shr-use-colors nil)
  (shr-use-fonts nil))

(use-package files
  :bind
  ("s-f" . find-file))

(require 'calc)
(require 'calc-units)
(require 'calc-yank)
(use-package calc
  ;; Tutorial: https://nullprogram.com/blog/2009/06/23/
  :config
  ;; https://old.reddit.com/r/emacs/comments/hujbbm/why_calceval_390010015_026_instead_585/
  (setq calc-multiplication-has-precedence nil)
  ;; https://www.n16f.net/blog/using-units-in-emacs-calc/
  (require 'calc-units)
  (setq math-additional-units
        '((b nil "Bit")
          (B "8 * b" "Byte")

          (kiB "2^10 * B" "Kibibyte")
          (MiB "2^20 * B" "Mebibyte")
          (GiB "2^30 * B" "Gibibyte")
          (TiB "2^40 * B" "Tebibyte")
          (PiB "2^50 * B" "Pebibyte")
          (EiB "2^60 * B" "Exbibyte")))
  ;; (setq math-units-table nil)           ; recalc. maybe not needed
  :custom
  (calc-show-banner nil)
  :bind
  ("<XF86Calculator>" . calc-dispatch)
  ("s-<XF86Calculator>" . calc)
  ("H-<XF86Calculator>" . calc-embedded)
  ("S-<XF86Calculator>" . calc-embedded-update-formula)
  (:map calc-mode-map
        ("H-d" . calc-pop)
        ("H-u" . calc-undo)
        ;; ([remap kill-this-buffer] . calc-quit))
        (:map calc-edit-mode-map
              ([remap save-kill-buffer] . calc-edit-finish)
              ([remap kill-this-buffer] . calc-edit-cancel))))

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
  (dired-vc-rename-file nil)) ; without this, some problems occurred

(use-package abbrev
  :defines (nix-repl-mode-abbrev-table inferior-emacs-lisp-mode-abbrev-table)
  :diminish abbrev-mode
  :hook
  (prog-mode . abbrev-mode)
  (comint-mode . abbrev-mode)
  (text-mode . abbrev-mode)
  :config
  ;; Abbreviations
  (define-abbrev global-abbrev-table "afaict" "as far as I can tell" nil :system t)
  (define-abbrev global-abbrev-table "btw" "by the way" nil :system t)
  (define-abbrev global-abbrev-table "pov" "point of view" nil :system t)
  (define-abbrev global-abbrev-table "truf" "truth" nil :system t)
  (define-abbrev global-abbrev-table "gr8" "great" nil :system t)
  (define-abbrev global-abbrev-table "thrf" "therefore" nil :system t)
  (define-abbrev global-abbrev-table "bcs" "because" nil :system t)
  ;; Typos
  (define-abbrev global-abbrev-table "wether" "whether" nil :system t)
  (define-abbrev global-abbrev-table "occured" "occurred" nil :system t)
  (define-abbrev global-abbrev-table "flase" "false" nil :system t)
  (define-abbrev global-abbrev-table "teh" "the" nil :system t)
  (define-abbrev global-abbrev-table "tehn" "then" nil :system t)
  (define-abbrev global-abbrev-table "fuond" "found" nil :system t)
  (define-abbrev global-abbrev-table "lnux" "linux" nil :system t)
  (define-abbrev global-abbrev-table "thsi" "this" nil :system t)
  (with-eval-after-load 'nix-repl
    (define-abbrev nix-repl-mode-abbrev-table "wpkgs" "with import <nixpkgs> { }; " nil :system t))
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

(use-package ielm
  :preface
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
  :bind
  ("M-s-→" . ielm)
  (:map inferior-emacs-lisp-mode-map
        ("H-ö" . ielm-send-input)
        ("M-ö" . ielm-send-input)
        ("<key-chord> f j" . ielm-send-input)
        ("s-." . eros-eval-last-sexp))
  :hook
  (ielm-mode . nagy-ielm-init-history)
  :custom
  (ielm-header "")
  :config
  (advice-add 'ielm-send-input :after #'nagy-ielm-write-history))

(use-package epg
  :custom
  (epg-pinentry-mode 'loopback))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0.01)
  ;; TODO increase eldoc delay for sly buffers because the comm with the lisp is
  ;; taking huge cpu.
  )

(use-package bookmark
  :init
  (setq bookmark-default-file (expand-file-name "~/.dotfiles/emacs-bookmarks")))

(use-package recentf
  :defer t
  :config
  ;; does not work in init
  (setq recentf-max-saved-items nil))

(use-package woman
  :defer t
  :custom
  (woman-fill-frame t)
  :hook
  (woman-mode . visual-fill-column-mode))

(use-package replace                    ; occur
  :bind
  (:map occur-edit-mode-map
        ([remap save-kill-buffer] . occur-cease-edit)
        ([remap kill-this-buffer] . occur-cease-edit))
  (:map occur-mode-map
        ("H-j" . occur-next)
        ("H-k" . occur-prev)))

;; (require 'rx)
(rx-define md5 (repeat 32 hex))
(rx-define sha1 (repeat 40 hex))
(rx-define sha256 (repeat 64 hex))
(rx-define sha512 (repeat 128 hex))

(use-package eww
  ;; could also bind this to `special-mode'
  :bind
  ("H-j" . scroll-up-command)
  ("H-k" . scroll-down-command))

(use-package elisp-mode
  :functions (ov-set)
  :preface
  (defun nagy-highlight-doom! ()
    "Highlight doom usage"
    (ov-set "add-hook!" 'face 'flymake-error)
    (ov-set "remove-hook!" 'face 'flymake-error)
    (ov-set "map!" 'face 'flymake-error))
  :hook
  (emacs-lisp-mode . nagy-highlight-doom!)
  :bind
  ("H-M-e" . emacs-lisp-mode))

(use-package mule-util
  :custom
  (truncate-string-ellipsis "┄")        ; use smaller char than default
  )

(provide 'nagy-emacs)
;;; nagy-emacs.el ends here
