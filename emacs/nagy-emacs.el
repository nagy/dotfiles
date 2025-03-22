;;; nagy-emacs.el --- config emacs packages -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "30.1") anaphora memoize ov reformatter zoom)
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

(defun nagy-emacs-split-window-below (arg)
  "Split the window vertically and focus the new window."
  (interactive "P")
  (if arg
      (split-root-window-below)
    (split-window-below))
  (if (and (eq major-mode 'exwm-mode) (called-interactively-p 'any))
      ;; for exwm compatibility
      (redisplay))
  (balance-windows))

(defun nagy-emacs-split-window-right (arg)
  "Split the window horizontally and focus the new window."
  (interactive "P")
  (if arg
      (split-root-window-right)
    (split-window-right))
  (if (and (eq major-mode 'exwm-mode) (called-interactively-p 'any))
      ;; for exwm compatibility
      (redisplay))
  (balance-windows))

;;;###autoload
(defmacro with-directory (dir &rest body)
  "Set `default-directory' to DIR and execute BODY."
  (declare (indent 1) (debug (sexp body)))
  `(let ((default-directory ,(if (stringp dir)
                                 (expand-file-name dir)
                               `(expand-file-name ,dir))))
     ,@body))

;; (defvar-keymap nagy-leader
;;   :doc "leader key")

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
  ;; Try to patch bug
  ;; More discussion about this: https://lists.gnu.org/archive/html/emacs-devel/2021-08/msg00363.html
  ;; More discussion about this: https://github.com/magit/magit/issues/4432
  ;; (make-variable-buffer-local 'revert-buffer-function)
  (setq-default indent-tabs-mode nil)
  (add-to-list 'default-frame-alist '(scroll-bar-width . 20))
  (tooltip-mode 0)
  (scroll-bar-mode 1)
  (fringe-mode 0)
  ;; this removes the yes/no question on a process killing
  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  ;; the #'enable-command function does not work. it causes a `~/.emacs` file to be created.
  (add-to-list 'display-buffer-alist '("^\\*Help" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*info" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Occur" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Async Shell Command"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Org-Babel"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Process"  display-buffer-same-window))
  ;; (add-to-list 'display-buffer-alist '("^\\*Embark Export"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\€"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Shell Command"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*compilation"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Diff"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Messages"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Disassemble"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Network Connection"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^Shell Command:"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^\\*Pp Eval Output"  display-buffer-same-window))
  (add-hook 'window-selection-change-functions #'nagy-emacs-window-scroll-bars)
  (advice-add 'text-scale-increase :after #'nagy-emacs-window-scroll-bars)
  (advice-add 'text-scale-decrease :after #'nagy-emacs-window-scroll-bars)
  (add-to-list 'set-message-functions 'inhibit-message)
  (setq inhibit-message-regexps
        (list
         (rx "[mu4e] ")
         (rx bol "Note: file is write protected" eol)
         ))
  (put #'erase-buffer 'disabled nil)
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
  (echo-keystrokes 0.02)          ; this has problems with `which-key'
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
  ;; using this instead of 10000 below means no flickering in the tab bar
  (tab-bar-auto-width-max `(,(* 8 880) ,(* 8 80)))
  ;; (tab-bar-auto-width-max '(10000 1000))
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
    (define-abbrev text-mode-abbrev-table "sh" "should" nil :system t)
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
  :diminish eldoc-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0.01)
  ;; TODO increase eldoc delay for sly buffers because the communication with the lisp is
  ;; taking huge cpu.
  ;; :same "^\\*eldoc\\*"
  )

(use-package bookmark
  :defer t
  :custom
  (bookmark-default-file (expand-file-name "~/.dotfiles/emacs-bookmarks"))
  ;; no general here
  ;; :general
  ;; (:states 'normal :keymaps 'bookmark-bmenu-mode-map
  ;;          "f" #'bookmark-bmenu-this-window)
  )

(use-package recentf
  :commands (recentf-keep-default-predicate)
  :preface
  (defun nagy-emacs-recentf--predicate (file)
    (pcase file
      ;; ((prefix "/nix/store") nil)
      (_ (recentf-keep-default-predicate file))))
  :defer t
  :custom
  (recentf-max-saved-items nil)
  (recentf-keep '(nagy-emacs-recentf--predicate))
  )

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
        ("H-k" . occur-prev))
  :general
  (:states 'normal :keymaps 'occur-edit-mode-map
           "ö" #'occur-cease-edit))

;; (require 'rx)
(rx-define md5 (repeat 32 hex))
(rx-define sha1 (repeat 40 hex))
(rx-define sha256 (repeat 64 hex))
(rx-define sha512 (repeat 128 hex))

(rx-define url
  (seq (or "http" "https") "://"
       (one-or-more (or word punctuation))
       (optional (seq ":" (one-or-more digit)))
       (optional (seq "/" (zero-or-more (or (syntax word) (syntax punctuation)))))
       (optional (seq "?" (one-or-more (or (syntax word) (syntax punctuation)))))
       (optional (seq "#" (one-or-more (or (syntax word) (syntax punctuation)))))))

;; (use-package xwidget
;;   :defer t
;;   :preface
;;   (defun nagy-emacs-xwidget-remove-header-line-h ()
;;     (setq-local header-line-format nil))
;;   (add-hook 'xwidget-webkit-mode-hook #'nagy-emacs-xwidget-remove-header-line-h)
;;   :config
;;   (remove-hook 'kill-buffer-query-functions #'xwidget-kill-buffer-query-function))

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
                " " filename)))
  :bind
  (:map ibuffer-mode-map
        ("H-d" . ibuffer-do-delete)))

(use-package elisp-mode
  :config
  (define-abbrev emacs-lisp-mode-abbrev-table "n" "nil" nil :system t :case-fixed t)
  :bind
  ("H-M-e" . emacs-lisp-mode)
  (:map emacs-lisp-mode-map
        ("s-." . eval-last-sexp)
        ;; ("s--" . eval-defun)
        ;; ("s-:" . eval-defun)
        ("C-ö" . compile-defun))
  :general
  (:states 'normal :keymaps 'emacs-lisp-mode-map
           "zd" #'narrow-to-defun
           "ö" #'eval-defun
           "Ö" #'eval-buffer
           ;; "µ" #'macrostep-expand
           )
  )

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
  :preface
  (defun nagy-emacs-project-find-file-flake-nix ()
    (interactive)
    (when (project-root (project-current))
      (find-file (concat (project-root (project-current)) "/flake.nix"))))
  (defun nagy-emacs-project-find-file-dot-git-config ()
    (interactive)
    (when (project-root (project-current))
      (find-file (concat (project-root (project-current)) "/.git/config"))))
  ;; :custom
  ;; (project-mode-line t)
  :config
  (keymap-global-set "H-p" project-prefix-map)
  (keymap-set project-prefix-map "s-k" #'project-kill-buffers)
  (keymap-set project-prefix-map "s-j" #'project-dired)
  (keymap-set project-prefix-map "1" #'nagy-emacs-project-find-file-flake-nix)
  (keymap-set project-prefix-map "2" #'nagy-emacs-project-find-file-dot-git-config)
  (keymap-global-set "s-," #'project-dired)
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

(defun nagy-length-buffer (orig-fun &rest args)
  (declare (side-effect-free t))        ; no more pure
  (cl-typecase (car args)
    (buffer (buffer-size (car args)))
    (window (buffer-size (window-buffer (car args))))
    (process (buffer-size (process-buffer (car args))))
    (overlay (- (overlay-end (car args)) (overlay-start (car args))))
    (frame (apply orig-fun (list (window-list (car args)))))
    (hash-table (hash-table-size (car args)))
    (t (apply orig-fun args))))
;; (advice-add 'length :around #'nagy-length-buffer)
;; (advice-remove 'length #'nagy-length-buffer)

(defun nagy-buffer-size (orig-fun &rest args)
  (declare (side-effect-free t))        ; no more pure
  (cl-typecase (car args)
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66890
    (string (setcar args (get-buffer (car args)))
            (apply orig-fun args))
    (window (setcar args (window-buffer (car args)))
            (apply orig-fun args))
    (process (setcar args (process-buffer (car args)))
             (apply orig-fun args))
    (overlay (setcar args (overlay-buffer (car args)))
             (apply orig-fun args))
    (marker (setcar args (marker-buffer (car args)))
            (apply orig-fun args))
    (t (apply orig-fun args))))
;; (advice-add 'buffer-size :around #'nagy-buffer-size)

(defun insert-multi (&rest args)
  (atomic-change-group
    (dolist (it args)
      (cl-typecase it
        (buffer (insert (with-current-buffer it
                          (buffer-string))))
        (window (insert (with-current-buffer (window-buffer it)
                          (buffer-string))))
        (process (insert (with-current-buffer (process-buffer it)
                           (buffer-string))))
        (t (insert it))))))

(defmacro toggle (arg)
  (declare (debug setq))
  `(setopt ,arg (not ,arg)))

(defalias 'wcb (symbol-function 'with-current-buffer))
(put 'wcb 'lisp-indent-function (get 'with-current-buffer 'lisp-indent-function))
(put 'wcb 'edebug-form-spec (get 'with-current-buffer 'edebug-form-spec))
(defalias 'wtb (symbol-function 'with-temp-buffer))
(put 'wtb 'lisp-indent-function (get 'with-temp-buffer 'lisp-indent-function))
(put 'wtb 'edebug-form-spec (get 'with-temp-buffer 'edebug-form-spec))
(defalias 'wsm (symbol-function 'with-silent-modifications))
(put 'wsm 'lisp-indent-function (get 'with-silent-modifications 'lisp-indent-function))
(defalias 'eb (symbol-function 'erase-buffer))
(defalias 'se (symbol-function 'save-excursion))
(put 'se 'lisp-indent-function (get 'save-excursion 'lisp-indent-function))
(defalias 'bs (symbol-function 'buffer-string))
(defalias 'bz (symbol-function 'buffer-size))
(defalias 'bn (symbol-function 'buffer-name))
(defalias 'gb (symbol-function 'get-buffer))
(defalias 'b (symbol-function 'get-buffer-create-or-current))
;; (defalias 'l (symbol-function 'length))
(defalias 'i (symbol-function 'insert-multi))
(defalias 'sw (symbol-function 'selected-window))
(defalias 'sf (symbol-function 'selected-frame))
(defalias 'wb (symbol-function 'window-buffer))
(defalias 'wl (symbol-function 'window-list))
(defalias 'bu (symbol-function 'browse-url))
(defalias 'pc (symbol-function 'pcase))
(defalias 'pce (symbol-function 'pcase-exhaustive))
(defalias 'P (symbol-function 'pcase))

(defalias 'plambda (symbol-function 'pcase-lambda))
(defalias 'plet (symbol-function 'pcase-let))
(defalias 'plet* (symbol-function 'pcase-let*))
(defalias 'pdolist (symbol-function 'pcase-dolist))

;; experiment with three letter shortcuts
(defalias 'win (symbol-function 'selected-window))
(defalias 'fra (symbol-function 'selected-frame))
(defalias 'buf (symbol-function 'get-buffer-create-or-current))
(defalias 'len (symbol-function 'length))
(defalias 'ins (symbol-function 'insert-multi))
(defalias 'bfs (symbol-function 'buffer-size))
(defalias 'bfn (symbol-function 'buffer-name))
(defalias 'callf (symbol-function 'cl-callf))
(defalias 'incf (symbol-function 'cl-incf))
(defalias 'decf (symbol-function 'cl-decf))
(defalias '+= (symbol-function 'cl-incf))
(defalias '-= (symbol-function 'cl-decf))
(defalias '++ (symbol-function 'cl-incf))
(defalias '-- (symbol-function 'cl-decf))
(defalias 'for (symbol-function 'cl-loop))
(defalias 'loop (symbol-function 'cl-loop))
(defalias 'assert (symbol-function 'cl-assert))
(defalias 'the (symbol-function 'cl-the))
(defalias 'progv (symbol-function 'cl-progv))
(defalias 'defstruct (symbol-function 'cl-defstruct))
(defalias 'defgeneric (symbol-function 'cl-defgeneric))
(defalias 'defmethod (symbol-function 'cl-defmethod))
(defalias 'typecase (symbol-function 'cl-typecase))
(defalias 'etypecase (symbol-function 'cl-etypecase))
(defalias 'oddp (symbol-function 'cl-oddp))
(defalias 'evenp (symbol-function 'cl-evenp))
(defalias 'first (symbol-function 'cl-first))
(defalias 'second (symbol-function 'cl-second))
(defalias 'copy-list (symbol-function 'cl-copy-list))

(defalias 'tg (symbol-function 'toggle))
(put 'tg 'edebug-form-spec (get 'toggle 'edebug-form-spec))

(defalias 'time< (symbol-function 'time-less-p))

(defalias 'ml (symbol-function 'map-length))
(defalias 'mk (symbol-function 'map-keys))
(defalias 'mv (symbol-function 'map-values))
(defalias 'mi (symbol-function 'map-insert))
(defalias 'me (symbol-function 'map-elt))
(put 'me 'gv-expander (get 'map-elt 'gv-expander))

;;;###autoload
(defmacro andf (place &rest x)
  (declare (debug (place form)))
  (if (symbolp place)
      `(setq ,place (and ,place ,@x))
    `(cl-callf and ,place ,@x)))

;;;###autoload
(defmacro orf (place &rest x)
  (declare (debug (place form)))
  (if (symbolp place)
      `(setq ,place (or ,place ,@x))
    `(cl-callf or ,place ,@x)))

;; (defvaralias 'mm 'major-mode)
;; (defvaralias 'dd 'default-directory)

(put 'thread-first 'lisp-indent-function 1)
(put 'thread-last 'lisp-indent-function 1)

(put 'vconcat 'pure t)

(put 'number-to-string 'pure t)
(put 'propertize 'pure t)

(put 'url-generic-parse-url 'pure t)
(put 'url-generic-parse-url 'side-effect-free t)

;; these may be derived by the compiler
;; (put 'string-trim-right 'pure t)
;; (put 'string-trim-right 'side-effect-free t)
;; (put 'substring-no-properties 'pure t)
;; (put 'substring-no-properties 'side-effect-free t)

(keymap-global-set "M-¢" #'cd)

(defmacro with-temp-file-contents (file &rest body)
  (declare (indent 1) (debug (sexp body)))
  (let ((sym (gensym)))
    `(let ((,sym ,file))
       (with-temp-buffer
         (insert-file-contents ,sym)
         (goto-char (point-min))
         ,@body))))

(use-package memoize
  ;; :demand t
  :config
  (setq memoize-default-timeout "5 minutes")
  )

(defmemoize-by-buffer-contents buffer-line-count-string ()
  ;; (count-lines (point-min) (point-max))
  (number-to-string
   (line-number-at-pos (point-max))))

;; (use-package bindat
;;   ;; :commands (bindat-pack bindat-type)
;;   :demand t
;;   )
;; (put 'bindat-pack 'pure t)
;; (put 'bindat-pack 'side-effect-free t)

(put 'line-number-at-pos 'side-effect-free t)
(put 'json-parse-string 'side-effect-free t)
(put 'json-parse-buffer 'side-effect-free t)

(use-package font-lock
  :bind
  ("M-⨏" . font-lock-update))

(use-package hl-line
  :bind
  ("M-ħ" . hl-line-mode))

(use-package outline
  :custom
  (outline-minor-mode-use-buttons t)
  ;; (outline-blank-line t)
  :bind
  ("H-s-a" . outline-cycle)
  ("H-s-i" . outline-show-all)
  ("H-s-o" . outline-hide-body)
  (:map outline-mode-prefix-map
        ("H-a" . outline-show-all)
        ("H-t" . outline-hide-body))
  (:map outline-overlay-button-map
        ("f" . outline-cycle))
  (:map outline-minor-mode-map
        ("H-a" . outline-cycle))
  :general
  (:states 'normal :keymaps 'outline-minor-mode-map
           "r" #'outline-cycle)
  ;; :hook
  ;; (conf-space-mode . outline-minor-mode)
  ;; :config
  ;; (map! "H-a" outline-mode-prefix-map)
  )

(defvar new-buffer--count 0)
(defun buffer-new-of-region ()
  (interactive)
  (let ((reg-str (when (region-active-p)
                   (buffer-substring (region-beginning)
                                     (region-end)))))
    (let ((buffer (generate-new-buffer (format "new%d" (cl-incf new-buffer--count)))))
      (with-current-buffer buffer
        (cd temporary-file-directory)
	(text-mode))
      (set-window-buffer nil buffer))
    (when reg-str
      (with-current-buffer (window-buffer (selected-window))
        (insert reg-str)
        (goto-char (point))))))
(keymap-global-set "C-s-SPC" #'buffer-new-of-region)

(defun buffer-new-of-kill ()
  (interactive)
  (let* ((buffer (generate-new-buffer (format "new%d" (cl-incf new-buffer--count))))
         (kill (current-kill 0))
         (is-json (ignore-errors
                    (ignore (json-parse-string kill))
                    t)))
    ;; (with-current-buffer buffer
    ;;   (text-mode))
    (set-window-buffer nil buffer)
    (awhen kill
      (with-current-buffer buffer
        (cd temporary-file-directory)
        (insert kill)
        (goto-char (point-min))
        (cond
         (is-json (js-json-mode) ;; (jq-format-buffer)
                  )
         (t (text-mode)))))))
(keymap-global-set "C-M-s-SPC" #'buffer-new-of-kill)

(require 'browse-url)
(defun my-browser-url-mpv (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (start-process (concat "mpv " url) nil "mpv" url)
  t)

(require 'url-parse)
(defun my-browser-url-nsxiv (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (url-unhex-string url))
  (let ((path (url-path-and-query (url-generic-parse-url url))))
    (with-environment-variables
        (("XDG_CACHE_HOME" "/run/user/1000/nsxiv-cache"))
      (awhen (cdr path)
        (setcar path (concat (car path) "?" it)))
      (start-process (concat "nsxiv " (car path)) nil "nsxiv" "-sf" (car path))))
  t)

(use-package ediff
  :defer t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package word-wrap-mode
  :config
  (global-word-wrap-whitespace-mode 1))

;; (fset 'yes-or-no-p #'y-or-n-p)

(use-package string-edit
  :bind
  (:map string-edit-mode-map
        ([remap save-kill-buffer] . string-edit-done)
        ([remap kill-this-buffer] . string-edit-abort)
        ([remap nagy-kill-this-buffer] . string-edit-abort)))

(use-package nxml-mode
  :preface
  (reformatter-define xml-format
    :group 'nxml
    :program "xml"
    :args '("format")
    :lighter " XmlFo")
  :bind
  ("H-M-x" . nxml-mode)
  (:map nxml-mode-map
        ("C-⊢" . xml-format-buffer))
  ;; :hook
  ;; (nxml-mode . xml-format-on-save-mode)
  )

(use-package timer-list
  :bind
  (:map timer-list-mode-map
        ("H-d" . timer-list-cancel)))

(use-package thingatpt
  :config
  ;; Re-evaluate to include equal-sign `=' into the list.
  ;; (defvar thing-at-point-file-name-chars "-@~/[:alnum:]_.${}#%,:=" "Characters allowable in filenames.")
  ;; (define-thing-chars filename thing-at-point-file-name-chars)
  )

(defun json-parse-file (file &rest args)
  "Read the first JSON object contained in FILE and return it."
  (declare (side-effect-free t))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (apply #'json-parse-buffer args)))

(use-package jsonrpc
  :cycle 'emacs-lisp-mode
  ("jsonrpc-request" "jsonrpc-async-request"))

(use-package package
  :custom
  (package-menu-async nil)
  :defer t
  ;; :config
  ;; (evil-set-initial-state 'package-menu-mode 'normal)
  )

;; This should be placed in (use-package emacs)
(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t)
  :config
  (minibuffer-depth-indicate-mode t)
  )

(provide 'nagy-emacs)
;;; nagy-emacs.el ends here
