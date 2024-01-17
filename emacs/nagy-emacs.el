;;; nagy-emacs.el --- config emacs packages -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: March 03, 2023
;; Modified: March 03, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy-emacs
;; Package-Requires: ((emacs "29.1") anaphora memoize ov visual-fill-column general)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Configuration of Emacs internal packages
;;
;;; Code:

(require 'general)
(require 'comint)
(require 'anaphora)
(require 'memoize)

(defun save-kill-buffer ()
  "Save and kill a buffer."
  (interactive)
  (save-buffer)
  (kill-buffer))

(defun nagy-emacs-split-window-below-and-focus (arg)
  "Split the window vertically and focus the new window."
  (interactive "P")
  (alet (if arg (split-root-window-below) (split-window-below))
    (if (and (eq major-mode 'exwm-mode) (called-interactively-p 'any))
        ;; for exwm compatibility
        (redisplay))
    (select-window it)
    (balance-windows)))

(defun nagy-emacs-split-window-right-and-focus (arg)
  "Split the window horizontally and focus the new window."
  (interactive "P")
  (alet (if arg (split-root-window-right) (split-window-right))
    (if (and (eq major-mode 'exwm-mode) (called-interactively-p 'any))
        ;; for exwm compatibility
        (redisplay))
    (select-window it)
    (balance-windows)))

(defmacro with-directory (dir &rest body)
  "Set `default-directory' to DIR and execute BODY."
  (declare (indent 1) (debug (sexp body)))
  `(let ((default-directory ,dir))
     ,@body))

(use-package emacs
  :preface
  (defun nagy-emacs-window-scroll-bars (&optional _win)
    (dolist (w (window-list))
      (if (string-equal "All" (format-mode-line "%p" nil w))
          (set-window-scroll-bars w nil nil 0 nil)
        (set-window-scroll-bars w nil t 0 nil))))
  :config
  (add-hook 'window-selection-change-functions #'nagy-emacs-window-scroll-bars)
  (advice-add 'text-scale-increase :after #'nagy-emacs-window-scroll-bars)
  (advice-add 'text-scale-decrease :after #'nagy-emacs-window-scroll-bars)
  ;; (setq-default show-trailing-whitespace t)
  ;; :custom
  ;; (help-at-pt-display-when-idle t)
  ;; (help-at-pt-timer-delay 0)
  :custom
  (inhibit-startup-screen t)
  (use-short-answers t)
  (message-log-max t)
  (x-stretch-cursor t)
  (kill-ring-max 250)
  (history-delete-duplicates t)
  (delete-by-moving-to-trash t)
  (load-prefer-newer t)
  (suggest-key-bindings nil)
  (large-file-warning-threshold (* 100 1000 1000))
  (duplicate-line-final-position -1)    ; to last line
  ;; (backtrace-on-redisplay-error t)
  (browse-url-default-scheme "https")
  (auto-save-default nil)
  (scroll-conservatively 101)
  (scroll-step 15)
  (scroll-bar-adjust-thumb-portion nil)
  ;; :config
  ;; (setq-default lexical-binding t) ;; has no effect yet
  :bind
  ("s-s" . nagy-emacs-split-window-below-and-focus)
  ("s-v" . nagy-emacs-split-window-right-and-focus)
  ("s-q" . bury-buffer)
  ("s-g" . keyboard-quit)
  ("s-=" . balance-windows)
  ("H-M-_" . fundamental-mode)
  ("H-M-t" . text-mode)
  ("H-e" . insert-char)
  ("C-H-e" . emoji-insert)
  ("H-r" . revert-buffer-quick)
  ("<XF86Reload>" . revert-buffer-quick)
  ("H-s-," . describe-char)
  ("H-s-." . display-local-help)
  ("H-s-:" . duplicate-dwim)
  ("M-s-ł" . find-library)
  ("H-s-m" . widen)
  ("H-0" . text-scale-adjust)
  ("H-+" . text-scale-increase)
  ("H--" . text-scale-decrease)
  ("s-<f8>" . scroll-bar-mode)
  ("S-<menu>" . execute-extended-command-for-buffer))

(use-package help
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
  ;; https://github.com/sulami/literate-calc-mode.el/issues/27#issuecomment-1218113511
  (defalias 'calcFunc-uconv 'math-convert-units)
  ;; (defalias 'calcFunc-urem 'math-remove-units)
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
  (tab-bar-auto-width-max '(330 30))
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice t))

;; exwm overlaps with tab line
;; (defun +nagy-window-edges-with-tab-line-mode (orig-fn &rest args)
;;   "Bug fix https://github.com/ch11ng/exwm/issues/788"
;;   (let ((result (apply orig-fn args)))
;;     (when (and tab-line-mode
;;                (equal '(t t t) (cdr args)))
;;       (cl-incf (cadr result) (frame-char-height)))
;;     result))
;; (advice-add 'window-edges :around #'+nagy-window-edges-with-tab-line-mode)
;; https://github.com/ch11ng/exwm/issues/788
;; (use-package tab-line
;;   :config
;;   (global-tab-line-mode 1)
;;   :custom
;;   (tab-line-close-button nil)
;;   (tab-line-new-button nil))

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
  (define-abbrev global-abbrev-table "gr8" "great" nil :system t)
  (define-abbrev global-abbrev-table "thrf" "therefore" nil :system t)
  ;; Typos
  (define-abbrev global-abbrev-table "wether" "whether" nil :system t)
  (define-abbrev global-abbrev-table "occured" "occurred" nil :system t)
  (define-abbrev global-abbrev-table "flase" "false" nil :system t)
  (define-abbrev global-abbrev-table "teh" "the" nil :system t)
  (define-abbrev global-abbrev-table "tehn" "then" nil :system t)
  (define-abbrev global-abbrev-table "fuond" "found" nil :system t)
  (define-abbrev global-abbrev-table "lnux" "linux" nil :system t)
  (define-abbrev global-abbrev-table "thsi" "this" nil :system t)
  (with-eval-after-load 'text-mode
    (define-abbrev text-mode-abbrev-table "gr" "great" nil :system t)
    (define-abbrev text-mode-abbrev-table "bc" "because" nil :system t)
    (define-abbrev text-mode-abbrev-table "lo" "long" nil :system t)
    (define-abbrev text-mode-abbrev-table "la" "last" nil :system t)
    (define-abbrev text-mode-abbrev-table "ev" "ever" nil :system t)
    (define-abbrev text-mode-abbrev-table "ng" "nothing" nil :system t)
    (define-abbrev text-mode-abbrev-table "hr" "here" nil :system t)
    (define-abbrev text-mode-abbrev-table "fo" "found" nil :system t)
    (define-abbrev text-mode-abbrev-table "aa" "against" nil :system t)
    (define-abbrev text-mode-abbrev-table "hb" "husband" nil :system t)
    (define-abbrev text-mode-abbrev-table "yo" "your" nil :system t)
    (define-abbrev text-mode-abbrev-table "tk" "think" nil :system t)
    (define-abbrev text-mode-abbrev-table "kg" "keeping" nil :system t)
    (define-abbrev text-mode-abbrev-table "dn" "down" nil :system t)
    (define-abbrev text-mode-abbrev-table "ai" "making" nil :system t)
    (define-abbrev text-mode-abbrev-table "xl" "human" nil :system t)
    ;; More https://jonaquino.blogspot.com/2007/06/yublin-shorthand-for-speed-writing.html?m=1
    ;; Idea: put yublin on QMK?
    (define-abbrev text-mode-abbrev-table "t" "the" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "n" "and" nil :system t)
    (define-abbrev text-mode-abbrev-table "w" "was" nil :system t)
    (define-abbrev text-mode-abbrev-table "h" "that" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "i" "his" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "e" "her" nil :system t)
    (define-abbrev text-mode-abbrev-table "y" "you" nil :system t)
    (define-abbrev text-mode-abbrev-table "d" "had" nil :system t)
    (define-abbrev text-mode-abbrev-table "b" "with" nil :system t)
    (define-abbrev text-mode-abbrev-table "f" "for" nil :system t)
    (define-abbrev text-mode-abbrev-table "s" "she" nil :system t)
    (define-abbrev text-mode-abbrev-table "o" "not" nil :system t)
    (define-abbrev text-mode-abbrev-table "u" "but" nil :system t)
    (define-abbrev text-mode-abbrev-table "v" "have" nil :system t)
    (define-abbrev text-mode-abbrev-table "m" "him" nil :system t)
    (define-abbrev text-mode-abbrev-table "c" "said" nil :system t)
    (define-abbrev text-mode-abbrev-table "g" "which" nil :system t)
    (define-abbrev text-mode-abbrev-table "j" "this" nil :system t)
    (define-abbrev text-mode-abbrev-table "l" "all" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "r" "from" nil :system t)
    (define-abbrev text-mode-abbrev-table "k" "they" nil :system t)
    (define-abbrev text-mode-abbrev-table "p" "were" nil :system t)
    (define-abbrev text-mode-abbrev-table "q" "would" nil :system t)
    (define-abbrev text-mode-abbrev-table "x" "when" nil :system t)
    (define-abbrev text-mode-abbrev-table "z" "what" nil :system t))
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
        ("s-." . eval-last-sexp))
  :hook
  (inferior-emacs-lisp-mode . nagy-ielm-init-history)
  ;; :custom
  ;; (ielm-header "")
  :config
  (setq ielm-header "")                 ; does not work in :custom because it is a defvar
  (advice-add 'ielm-send-input :after #'nagy-ielm-write-history))

(use-package epg
  :custom
  (epg-pinentry-mode 'loopback))

(use-package eldoc
  :diminish 'eldoc-mode
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
  ;; does not work in init because of doom
  (setq recentf-max-saved-items nil))

(use-package woman
  :defer t
  :custom
  (woman-fill-frame t))

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

(require 'xwidget)
(use-package xwidget
  :preface
  (defun nagy-emacs-xwidget-remove-header-line-h ()
    (setq-local header-line-format nil))
  (add-hook 'xwidget-webkit-mode-hook #'nagy-emacs-xwidget-remove-header-line-h)
  :config
  (remove-hook 'kill-buffer-query-functions #'xwidget-kill-buffer-query-function))

(use-package eww
  ;; could also bind this to `special-mode'
  :bind
  ("H-j" . scroll-up-command)
  ("H-k" . scroll-down-command))

(use-package epg
  :config
  ;; Known problems with gpg 2.4.1
  ;; https://stackoverflow.com/questions/76388376/emacs-org-encrypt-entry-hangs-when-file-is-modified
  (fset 'epg-wait-for-status 'ignore))

(use-package ibuffer
  :custom
  (ibuffer-expert t)
  :config
  (setq ibuffer-formats
        '((mark modified read-only locked " "
           (name 42 42 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

(use-package elisp-mode
  :functions (ov-set)
  :preface
  (defun nagy-emacs-highlight-doom! ()
    "Highlight doom usage"
    (ov-set (rx (or "add-hook!" "remove-hook!" "map!"))
            'face 'flymake-error))
  :hook
  (emacs-lisp-mode . nagy-emacs-highlight-doom!)
  :bind
  ("H-M-e" . emacs-lisp-mode))

(use-package mule-util
  :defer t
  :config
  ;; Does not work in custom and needs :defer t
  (setq truncate-string-ellipsis "┄")        ; use smaller char than default
  )

(use-package paren
  :custom
  (show-paren-delay 0.0)
  :config
  (set-face-attribute 'show-paren-match nil :inherit 'modus-themes-subtle-blue :background 'unspecified))

(use-package winner
  :bind
  ("s-u" . winner-undo)
  ("s-U" . winner-redo))

(use-package project
  :bind
  ("C-s-<return>" . project-eshell)
  ("C-H-s--" . project-dired))

(keymap-global-set "<mouse-8>" #'bury-buffer)
(keymap-global-set "<mouse-9>" #'unbury-buffer)

(provide 'nagy-emacs)
;;; nagy-emacs.el ends here
