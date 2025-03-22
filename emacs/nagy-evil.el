;;; nagy-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: December 01, 2022
;; Modified: December 01, 2022
;; Version: 0.0.1
;; Keywords: extensions
;; Homepage: https://github.com/nagy/nagy-evil
;; Package-Requires: ((emacs "30.1") evil evil-collection evil-escape eat evil-numbers evil-surround evil-matchit evil-goggles evil-nerd-commenter olivetti ws-butler key-chord vertico nagy-use-package)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'diminish)

(defvar evil-want-keybinding nil)
(defvar evil-want-C-u-scroll t)
;; This likely needs to be set early
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(require 'evil)
(require 'evil-escape)
(require 'general)

(use-package evil
  :init
  ;; This is for evil-collection to work properly
  ;; This likely needs to be in :init
  (setq evil-want-keybinding nil)
  :demand t
  :commands (+evil-indent-whole-buffer)
  :custom
  (evil-normal-state-cursor '(hbar . 5))
  (evil-insert-state-cursor '(bar . 5))
  (evil-echo-state nil)
  (evil-mode-line-format nil)
  (evil-want-minibuffer t)
  (evil-undo-system 'undo-redo)
  ;; This is for evil-collection to work properly
  ;; (evil-want-keybinding nil)
  ;; Does not work because deletion commands also are affected
  ;; (evil-respect-visual-line-mode t)
  :config
  (defun +evil-indent-whole-buffer ()
    "Evil indent the whole buffer"
    (interactive)
    (save-excursion
      (evil-indent (point-min) (point-max))))
  (keymap-global-set "H-9" #'+evil-indent-whole-buffer)
  (evil-mode 1)
  (evil-global-set-key 'motion (kbd "TAB") nil) ; allows magit-section-toggle to be accessible
  (evil-global-set-key 'insert "\C-k" nil) ; was evil-insert-digraph
  ;; (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  (evil-define-key 'normal minibuffer-mode-map (kbd "RET") #'exit-minibuffer)
  (evil-define-key 'insert minibuffer-mode-map (kbd "C-g") #'minibuffer-keyboard-quit)
  (evil-global-set-key 'insert (kbd "C-a") #'beginning-of-line)
  (evil-global-set-key 'insert (kbd "C-e") #'end-of-line)
  :bind
  ("H-z" . evil-scroll-line-to-center)
  ;; ("H-u" . evil-undo)
  )

(use-package evil-collection
  :demand t
  :commands (evil-collection-init)
  :config
  ;; This is for evil-collection to work properly
  ;; (setq evil-want-keybinding nil)
  (setq evil-collection-want-unimpaired-p nil)
  (evil-collection-init
   '(magit
     ibuffer
     dired
     mu4e
     debug
     help
     edebug
     emms
     elfeed
     pass
     ediff
     macrostep
     man
     woman
     process-menu
     package-menu
     timer-list
     compile
     info
     arc-mode
     tar-mode
     custom
     pdf
     org
     profiler
     xref
     apropos
     calendar
     markdown-mode
     image-dired
     replace                            ; for occur
     ;; gnus
     bookmark
     vc-dir
     vc-git
     doc-view
     ))
  )

(use-package evil-numbers
  :preface
  (defun nagy-evil-numbers-inc-10 (arg)
    (interactive "p")
    (setq arg (or arg 1))
    (evil-numbers/inc-at-pt-incremental (* arg 10) nil))
  (defun nagy-evil-numbers-dec-10 (arg)
    (interactive "p")
    (setq arg (or arg 1))
    (evil-numbers/dec-at-pt-incremental (* arg 10) nil))
  ;; :bind
  ;; ("H-<up>" . evil-numbers/inc-at-pt-incremental)
  ;; ("H-<down>" . evil-numbers/dec-at-pt-incremental)
  :general
  (:states 'normal :keymaps '(prog-mode-map text-mode-map)
           "↑" #'evil-numbers/inc-at-pt-incremental
           "↓" #'evil-numbers/dec-at-pt-incremental
           "→" #'nagy-evil-numbers-inc-10
           "←" #'nagy-evil-numbers-dec-10
           "g +" #'evil-numbers/inc-at-pt-incremental
           "g -" #'evil-numbers/dec-at-pt-incremental)
  )

(use-package eshell
  :defer t
  :custom
  (eshell-banner-message "")
  ;; (eshell-scroll-to-bottom-on-output nil)
  ;; :general
  ;; (:states 'normal :keymaps 'eshell-mode-map
  ;;          "k" #'evil-previous-visual-line
  ;;          "j" #'evil-next-visual-line
  ;;          "ö" #'eshell-send-input
  ;;          "Ö" #'eshell-previous-input)
  )

(use-package evil-surround
  :bind
  ("H-(" . evil-surround-region)
  ("H-)" . evil-surround-delete))

(use-package shell
  :defer t
  :config
  (evil-set-initial-state 'shell-command-mode 'normal)
  ;; (evil-set-initial-state 'shell-command-mode 'emacs)
  )

(use-package sqlite-mode
  ;; Tracking issue https://github.com/emacs-evil/evil-collection/issues/749
  :general
  (:states 'normal :keymaps 'sqlite-mode-map
           "H-r" #'sqlite-mode-list-tables
           "c" #'sqlite-mode-list-columns
           "f" #'sqlite-mode-list-data
           "RET" #'sqlite-mode-list-data)
  ;; :config
  ;; (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . nagy-sqlite-view-file-magically))
  :same "^\\*SQLite ")

(use-package sql
  :abbrev 'sql-mode
  ("c" . "create")
  ("t" . "table")
  ("s" . "select")
  ("i" . "insert")
  ("u" . "update")
  ("f" . "from")
  ("d" . "delete")
  ("dr" . "drop")
  ("e" . "exists")
  :bind
  ("H-M-q" . sql-mode))

;; (use-package evil-collection-calc
;;   :config
;;   (evil-set-initial-state 'calc-mode 'emacs))

(use-package eat
  :functions (eat-ncdu eat-dool)
  :preface
  (defun nagy--eat-char-mode (_proc)
    (eat-char-mode)
    (setq-local truncate-lines t))
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-directory-tracking nil)
  (eat-enable-shell-prompt-annotation nil)
  (eat-term-scrollback-size nil)
  :config
  ;; (setq process-adaptive-read-buffering nil)
  (evil-set-initial-state 'eat-mode 'emacs)
  (add-hook 'eat-exec-hook #'nagy--eat-char-mode)
  (defun eat-ncdu ()
    "Doctext."
    (interactive)
    (let ((eat-buffer-name "*eat-ncdu*")
          (eat-kill-buffer-on-exit t))
      (when (get-buffer eat-buffer-name)
        (kill-buffer (get-buffer eat-buffer-name)))
      (eat "ncdu")))
  (defun eat-dool ()
    "Doctext."
    (interactive)
    (let ((eat-buffer-name "*eat-dool*")
          (eat-kill-buffer-on-exit t))
      (eat "dool -N eth0 --bytes --bw")))
  :bind
  (:map evil-normal-state-map
        ("<key-chord> - x" . eat)
        ("<key-chord> ß w" . eat-ncdu))
  (:map eat-mode-map
        ("H-h" . eat-emacs-mode)
        ("H-l" . eat-char-mode)
        ("H-r" . eat-reset)
        ))

(require 'vertico)
(use-package key-chord
  :preface
  (defun nagy-evil-minibuffer-set-home ()
    (interactive)
    (delete-minibuffer-contents)
    (goto-char (point-max))
    (insert "~/"))
  (defun nagy-evil-minibuffer-set-tmp ()
    (interactive)
    (delete-minibuffer-contents)
    (goto-char (point-max))
    (insert temporary-file-directory))
  (defun nagy-evil-minibuffer-set-nix ()
    (interactive)
    (delete-minibuffer-contents)
    (goto-char (point-max))
    (insert
     ;; (concat nix-store-dir "/")
     "/nix/store/"
     ))
  :after evil
  :demand t
  :commands (key-chord-define key-chord-mode)
  :custom
  (key-chord-one-key-delay 0.4)
  (key-chord-two-keys-delay 0.2)
  (key-chord-safety-interval-backward 0.0)
  (key-chord-safety-interval-forward 0.0)
  :bind
  (:map evil-normal-state-map
        ("<key-chord> - <" . vertico-flat-mode)
        ("<key-chord> - c" . eshell)
        ("<key-chord> - h" . consult-imenu)
        ("<key-chord> - w" . global-prettify-symbols-mode))
  (:map vertico-map
        ("<key-chord> f j" . vertico-exit)
        ("<key-chord> j f" . minibuffer-keyboard-quit))
  (:map minibuffer-local-map
        ("<normal-state> <key-chord> - h" . nagy-evil-minibuffer-set-home)
        ("<normal-state> <key-chord> - t" . nagy-evil-minibuffer-set-tmp)
        ("<normal-state> <key-chord> - n" . nagy-evil-minibuffer-set-nix)
        ("<key-chord> f j" . exit-minibuffer)
        ("<key-chord> j f" . minibuffer-keyboard-quit))
  ("s-SPC" . exit-minibuffer)
  ("H-SPC" . minibuffer-keyboard-quit)
  (:map evil-ex-search-keymap
        ("<key-chord> f j" . exit-minibuffer)
        ("<key-chord> j f" . minibuffer-keyboard-quit))
  ;; (:map emacs-lisp-mode-map
  ;;       ("<insert-state> <key-chord> f l" . indent-for-tab-command))
  :config
  (key-chord-mode 1)
  ;; (global-set-key (kbd "<key-chord> - <") #'vertico-flat-mode)  ; this causes "-" to be slow while in insert mode
  (key-chord-define evil-insert-state-map  "jk" #'evil-normal-state)
  ;; (key-chord-define evil-insert-state-map  "kj" #'evil-normal-state) ; may be a bit overkill
  ;; (key-chord-define evil-emacs-state-map "jk" #'evil-normal-state) ; makes eat terminal slow

  :general
  (:states 'normal
           ;; Was evil-repeat-pop and evil-repeat-pop-next. I dont understand these commands yet, so lets
           ;; keep the binding available.
           "C-." nil
           "M-." nil ;; This opens up M-. back to xref-find-definitions
           )
  )

(use-package tar-mode
  :general
  (:states 'normal :keymaps 'tar-mode-map
           "f" #'tar-extract))

(require 'evil-goggles)
(use-package evil-goggles
  :diminish 'evil-goggles-mode
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  :config
  (cl-loop for it in '((evil-magit-yank-whole-line :face evil-goggles-yank-face
                                                   :switch evil-goggles-enable-yank
                                                   :advice evil-goggles--generic-async-advice)
                       (+evil:yank-unindented :face evil-goggles-yank-face
                                              :switch evil-goggles-enable-yank
                                              :advice evil-goggles--generic-async-advice)
                       (+eval:region :face evil-goggles-yank-face
                                     :switch evil-goggles-enable-yank
                                     :advice evil-goggles--generic-async-advice)
                       (lispyville-delete :face evil-goggles-delete-face
                                          :switch evil-goggles-enable-delete
                                          :advice evil-goggles--generic-blocking-advice)
                       (lispyville-delete-line :face evil-goggles-delete-face
                                               :switch evil-goggles-enable-delete
                                               :advice evil-goggles--delete-line-advice)
                       (lispyville-yank :face evil-goggles-yank-face
                                        :switch evil-goggles-enable-yank
                                        :advice evil-goggles--generic-async-advice)
                       ;; (nagy/yank-paragraph :face evil-goggles-yank-face
                       ;;                      :switch evil-goggles-enable-yank
                       ;;                      :advice evil-goggles--generic-async-advice)
                       (lispyville-yank-line :face evil-goggles-yank-face
                                             :switch evil-goggles-enable-yank
                                             :advice evil-goggles--generic-async-advice)
                       (lispyville-change :face evil-goggles-change-face
                                          :switch evil-goggles-enable-change
                                          :advice evil-goggles--generic-blocking-advice)
                       (lispyville-change-line :face evil-goggles-change-face
                                               :switch evil-goggles-enable-change
                                               :advice evil-goggles--generic-blocking-advice)
                       (lispyville-change-whole-line :face evil-goggles-change-face
                                                     :switch evil-goggles-enable-change
                                                     :advice evil-goggles--generic-blocking-advice)
                       (lispyville-indent :face evil-goggles-indent-face
                                          :switch evil-goggles-enable-indent
                                          :advice evil-goggles--generic-async-advice)
                       (lispyville-join :face evil-goggles-join-face
                                        :switch evil-goggles-enable-join
                                        :advice evil-goggles--join-advice))
           do
           (add-to-list 'evil-goggles--commands it))
  (evil-goggles-mode 1))

(defun show-date()
  "Show the current date as a message."
  (interactive)
  ;; prevent tramp problems
  (let ((default-directory temporary-file-directory))
    (message (string-trim-right (shell-command-to-string "date")))))
(keymap-global-set "s-⌚" #'show-date)
(keymap-global-set "s-⧖" #'show-date)

(keymap-set evil-normal-state-map "<key-chord> y SPC" #'duplicate-dwim)
(keymap-set evil-normal-state-map "ü" #'execute-extended-command)
(keymap-set evil-normal-state-map "Ü" #'execute-extended-command-for-buffer)
(keymap-set evil-normal-state-map "⋮" #'sort-lines)
;; (keymap-set evil-motion-state-map "TAB" nil)
;; (keymap-set evil-motion-state-map "RET" nil)

(keymap-set evil-normal-state-map "ë" #'global-prettify-symbols-mode)
;; (keymap-global-set "C-ë" #'nameless-mode)

(use-package olivetti
  :custom
  (olivetti-body-width 150)
  :general
  (:states 'normal
           "“" #'olivetti-expand
           "”" #'olivetti-shrink))

(use-package ws-butler
  :diminish ws-butler-mode
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :hook
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))

(use-package wdired
  :defer t
  :config
  (evil-set-initial-state 'wdired-mode 'normal))

(provide 'nagy-evil)
;;; nagy-evil.el ends here
