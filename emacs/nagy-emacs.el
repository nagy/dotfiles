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
;; Package-Requires: ((emacs "29.1") anaphora memoize ov visual-fill-column reformatter general)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Configuration of Emacs internal packages
;;
;;; Code:

(require 'general)
;; (require 'comint)
;; (require 'anaphora)
(require 'memoize)

;; (require 'reformatter)

(defun save-kill-buffer ()
  "Save and kill a buffer."
  (interactive)
  (save-buffer)
  (kill-buffer))

;; from https://gist.github.com/3402786
(defun spacemacs/toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn                                ; implicit
        (window-configuration-to-register ?_)
        (delete-other-windows)))))
(keymap-global-set "s-m" #'spacemacs/toggle-maximize-buffer)

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

;;;###autoload
(defmacro with-directory (dir &rest body)
  "Set `default-directory' to DIR and execute BODY."
  (declare (indent 1) (debug (sexp body)))
  `(let ((default-directory ,(if (stringp dir)
                                 (expand-file-name dir)
                               `(expand-file-name ,dir))))
     ,@body))

(defvar-keymap nagy-leader
  :doc "leader key")

(use-package emacs
  :preface
  (defun nagy-emacs-window-scroll-bars (&optional _win)
    (dolist (w (window-list))
      (if (string-equal "All" (format-mode-line "%p" nil w))
          (set-window-scroll-bars w nil nil 0 nil)
        (set-window-scroll-bars w nil t 0 nil))))
  ;; From Doom:
  ;; FIX: The native border "consumes" a pixel of the fringe on righter-most
  ;;   splits, `window-divider' does not. Available since Emacs 25.1.
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (add-hook 'after-init-hook #'window-divider-mode)
  :config
  (setq-default indent-tabs-mode nil)
  (add-to-list 'default-frame-alist '(scroll-bar-width . 20))
  (tooltip-mode 0)
  (scroll-bar-mode 1)
  (fringe-mode 0)
  ;; this removes the yes/no question on a process killing
  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  ;; the #'enable-command function does not work. it causes a `~/.emacs` file to be created.
  (push '("^\\*Help" display-buffer-same-window) display-buffer-alist)
  (push '("^\\*info" display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Occur" display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Async Shell Command"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Org-Babel"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Process"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Embark Export"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\€"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Shell Command"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\*compilation"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Diff"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Messages"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Disassemble"  display-buffer-same-window) display-buffer-alist)
  (push '("^\\*Network Connection"  display-buffer-same-window) display-buffer-alist)
  (push '("^Shell Command:"  display-buffer-same-window) display-buffer-alist)
  (add-hook 'window-selection-change-functions #'nagy-emacs-window-scroll-bars)
  (advice-add 'text-scale-increase :after #'nagy-emacs-window-scroll-bars)
  (advice-add 'text-scale-decrease :after #'nagy-emacs-window-scroll-bars)
  (push 'inhibit-message set-message-functions)
  (setq inhibit-message-regexps
        (list
         (rx "[mu4e] ")
         (rx bol "Note: file is write protected" eol)
         ))

  ;; (setq-default show-trailing-whitespace t)
  ;; :custom
  ;; (help-at-pt-display-when-idle t)
  ;; (help-at-pt-timer-delay 0)
  :custom
  ;; (fill-column 150)
  (inhibit-startup-screen t)
  (use-short-answers t)
  (message-log-max t)
  (x-stretch-cursor t)
  (kill-ring-max 250)
  (history-delete-duplicates t)
  (delete-by-moving-to-trash t)
  (load-prefer-newer t)
  (suggest-key-bindings nil)
  (large-file-warning-threshold (* 1000 1000 1000))
  (duplicate-line-final-position -1)    ; to last line
  ;; (backtrace-on-redisplay-error t)
  (browse-url-default-scheme "https")
  (auto-save-default nil)
  (scroll-conservatively 101)
  (scroll-step 15)
  (scroll-bar-adjust-thumb-portion nil)
  (initial-scratch-message nil)
  (debugger-stack-frame-as-list t)
  (read-minibuffer-restore-windows nil)
  ;; this disables the blinking cursor in the terminal; blink-mode is not enough.
  (visible-cursor nil)
  (echo-keystrokes 0.02)                ; this has problems with `which-key'
  ;; (uniquify-buffer-name-style nil)
  (cursor-in-non-selected-windows nil)
  (create-lockfiles nil)
  (make-backup-files nil)
  (echo-keystrokes-help nil)            ; emacs 30
  ;; (image-scaling-factor 2.0)
  (browse-url-firefox-program "firefox-esr")
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  ;; (keymap-global-set "H-m" nagy-leader)
  ;; (setq-default lexical-binding t) ;; has no effect yet
  :bind
  ("s-s" . nagy-emacs-split-window-below-and-focus)
  ("s-v" . nagy-emacs-split-window-right-and-focus)
  ("s-q" . bury-buffer)
  ("s-k" . kill-this-buffer)
  ("M-H" . mark-whole-buffer)
  ("s-w" . save-buffer)
  ("s-z" . save-kill-buffer)
  ("s-g" . keyboard-quit)
  ("s-e" . browse-url)
  ("s-=" . balance-windows)
  ("H-M-_" . fundamental-mode)
  ("H-M-t" . text-mode)
  ("H-e" . insert-char)
  ("C-H-e" . emoji-insert)
  ("H-r" . revert-buffer-quick)
  ("<XF86Reload>" . revert-buffer-quick)
  ("<XF86Back>" . tab-previous)
  ("<XF86Forward>" . tab-next)
  ("M-ð" . disassemble)
  ("H-s-," . describe-char)
  ("H-s-." . display-local-help)
  ("H-s-:" . duplicate-dwim)
  ("M-s-ł" . find-library)
  ("H-s-m" . widen)
  ("H-0" . text-scale-adjust)
  ("H-+" . text-scale-increase)
  ("H--" . text-scale-decrease)
  ("s-<f8>" . scroll-bar-mode)
  ("S-<menu>" . execute-extended-command-for-buffer)
  ("s-n" . universal-argument)
  ("A-C-s-}" . tab-duplicate)
  ("C-H-r" . rename-buffer)
  ("H-M-a" . normal-mode)
  (:map minibuffer-local-map
        ("H-j" . next-history-element)
        ("H-k" . previous-history-element)
        ("s-g" . minibuffer-keyboard-quit))
  (:map universal-argument-map
        ("s-n" . universal-argument-more))
  (:map y-or-n-p-map
        ([remap save-kill-buffer] . y-or-n-p-insert-y)
        ([remap kill-this-buffer] . minibuffer-keyboard-quit)
        ([remap embark-act] . y-or-n-p-insert-y))
  (:map help-map
        ("C-k" . describe-key-briefly)))

(use-package help
  :bind
  ("C-H-h" . describe-key-briefly)
  (:map help-map
        ("C-l" . find-library)))

(use-package repeat
  :bind
  ("M-s-." . repeat))

(use-package man
  :custom
  (Man-notify-method 'pushy)
  :bind
  (:map Man-mode-map
        ("H-j" . Man-next-section)
        ("H-k" . Man-previous-section)))

(use-package simple
  :preface
  (defun nagy-emacs-process-menu-dired-jump ()
    (interactive)
    (and-let* ((fourth-col (elt (tabulated-list-get-entry (point)) 3))
               (buffer (plist-get (cdr fourth-col) 'process-buffer)))
      (switch-to-buffer buffer)))
  :bind
  (:map process-menu-mode-map
        ([remap dired-jump] . nagy-emacs-process-menu-dired-jump)
        ("H-d" . process-menu-delete-process))
  :diminish 'visual-line-mode)

(use-package compile
  ;; :custom
  ;; (compilation-scroll-output t)
  :bind
  ("C-M-¢" . compile)
  ("H-s-j" . next-error)
  ("H-s-k" . previous-error)
  ("H-s-c" . compile)
  (:map compilation-minor-mode-map
        ([remap revert-buffer-quick] . recompile)
        ("H-j" . compilation-next-error)
        ("H-k" . compilation-previous-error)))

(use-package tabulated-list
  ;; :custom
  ;; (text-scale-remap-header-line t)
  :bind
  (:map tabulated-list-mode-map
        ("M-s M-s" . tabulated-list-sort)
        ("<normal-state> →" . tabulated-list-next-column)
        ("<normal-state> ←" . tabulated-list-previous-column)))

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
        ;; ("H-u" . calc-undo)
        ;; ([remap kill-this-buffer] . calc-quit))
        (:map calc-edit-mode-map
              ([remap save-kill-buffer] . calc-edit-finish)
              ([remap kill-this-buffer] . calc-edit-cancel)
              ([remap nagy-kill-this-buffer] . calc-edit-cancel))))

(use-package tab-bar
  :config
  (tab-bar-mode)
  :custom
  (tab-bar-show 1)
  (tab-bar-auto-width t)
  ;; (tab-bar-auto-width-max '(330 30))
  ;; (tab-bar-auto-width-max '(440 40))
  (tab-bar-auto-width-max '(10000 1000))
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
  (wdired-allow-to-change-permissions t)
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
    (define-abbrev text-mode-abbrev-table "n" "and" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "w" "was" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "h" "that" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "i" "his" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "e" "her" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "y" "you" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "d" "had" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "b" "with" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "f" "for" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "s" "she" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "o" "not" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "u" "but" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "v" "have" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "m" "him" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "c" "said" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "g" "which" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "j" "this" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "l" "all" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "r" "from" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "k" "they" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "p" "were" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "q" "would" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "x" "when" nil :system t :case-fixed t)
    (define-abbrev text-mode-abbrev-table "z" "what" nil :system t :case-fixed t))
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
  :preface
  ;; this is starting to need tests
  (defun nagy-emacs-hilock-highlight ()
    ;; we fake, that font-lock-mode is enabled.
    ;; this makes these rules dynamic while editing.
    (let ((font-lock-mode 1)
          (font-lock-defaults '((nil))))
      (when (derived-mode-p 'emacs-lisp-mode)
        (hi-lock-face-buffer ".*\\(map!\\).*" 'modus-themes-intense-red 1))
      (hi-lock-face-buffer ".* \\([@][@]\\) .*" 'modus-themes-subtle-green 0)
      (hi-lock-face-buffer ".* \\([!][!]\\) .*" 'modus-themes-subtle-red 0)
      (hi-lock-face-buffer ".* \\([?][?]\\) .*" 'modus-themes-subtle-yellow 0)))
  ;; :config
  ;; (add-hook 'font-lock-mode-hook 'nagy-emacs-hilock-highlight 'append)
  ;; :hook
  ;; (font-lock-mode . nagy-emacs-hilock-highlight)
  :diminish 'hi-lock-mode
  :bind
  ("A-s-H-." . highlight-symbol-at-point))

(use-package ielm
  :bind
  ("M-s-→" . ielm)
  (:map inferior-emacs-lisp-mode-map
        ("H-ö" . ielm-send-input)
        ("H-b" . ielm-change-working-buffer)
        ("M-ö" . ielm-send-input)
        ("<key-chord> f j" . ielm-send-input)
        ("s-." . eval-last-sexp))
  :config
  (setq ielm-header "") ; does not work in :custom because it is a defvar
  )

(use-package epg
  :defer t
  :custom
  (epg-pinentry-mode 'loopback))

(use-package eldoc
  :preface
  ;; from Doom emacs
  (defun doom-emacs-lisp-append-value-to-eldoc-a (fn sym)
    "Display variable value next to documentation in eldoc."
    (when-let (ret (funcall fn sym))
      (if (boundp sym)
          (concat ret " "
                  (let* ((truncated " [...]")
                         (print-escape-newlines t)
                         (str (symbol-value sym))
                         (str (prin1-to-string str))
                         (limit (- (frame-width) (length ret) (length truncated) 1)))
                    (format (format "%%0.%ds%%s" (max limit 0))
                            (propertize str 'face 'warning)
                            (if (< (length str) limit) "" truncated))))
        ret)))
  ;; :demand t
  ;; :after 'elisp-mode
  :config
  (advice-add 'elisp-get-var-docstring :around #'doom-emacs-lisp-append-value-to-eldoc-a)
  :diminish 'eldoc-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0.01)
  ;; TODO increase eldoc delay for sly buffers because the comm with the lisp is
  ;; taking huge cpu.
  ;; :same "^\\*eldoc\\*"
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
  :defer t
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
  ;; :defer t
  :config
  ;; Does not work in custom and needs :defer t
  (setq truncate-string-ellipsis "┄")        ; use smaller char than default
  )

(use-package paren
  :custom
  (show-paren-delay 0.0)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
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

(use-package debug
  :hook
  (debugger-mode . visual-line-mode)
  :bind
  (:map debugger-mode-map
        ("H-w" . edebug-where)))

(use-package message
  :bind
  (:map message-mode-map
        ([remap save-kill-buffer] . message-send-and-exit)
        ([remap kill-this-buffer] . message-kill-buffer)))

(use-package autorevert
  :diminish auto-revert-mode
  :custom
  (auto-revert-verbose nil)
  :hook
  (dired-mode . auto-revert-mode)
  :bind
  ("C-⧖" . auto-revert-mode))

(keymap-global-set "<mouse-8>" #'bury-buffer)
(keymap-global-set "<mouse-9>" #'unbury-buffer)

(defun find-file-home () "To ~/" (interactive) (find-file "~/"))
(keymap-global-set "<XF86HomePage>" #'find-file-home)
(keymap-global-set "H-#" #'other-window-prefix)
(keymap-global-set "H-s-#" #'other-tab-prefix)

(defun get-buffer-create-or-current (&optional object)
  (declare (side-effect-free t))
  (cl-etypecase object
    (null (current-buffer))
    (string (get-buffer-create object))
    (buffer object)
    (process (process-buffer object))
    (window (window-buffer object))
    (marker (marker-buffer object))
    (overlay (overlay-buffer object))))

(use-package timer-list
  :bind
  (:map timer-list-mode-map
        ("H-d" . timer-list-cancel)))

(defun json-parse-file (file &rest args)
  (declare (side-effect-free t))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (apply #'json-parse-buffer args)))

(use-package jsonrpc
  :cycle 'emacs-lisp-mode
  ("jsonrpc-request" "jsonrpc-async-request"))

(provide 'nagy-emacs)
;;; nagy-emacs.el ends here
