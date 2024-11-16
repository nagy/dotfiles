;;; nagy-evil.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
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
;; Package-Requires: ((emacs "29.1") evil evil-collection evil-escape eat evil-numbers evil-surround evil-goggles key-chord vertico general nagy-use-package)
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
  (evil-set-initial-state 'shell-mode 'normal))

(use-package sqlite-mode
  ;; Tracking issue https://github.com/emacs-evil/evil-collection/issues/749
  :general
  (:states 'normal :keymaps 'sqlite-mode-map
           "H-r" #'sqlite-mode-list-tables
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
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-directory-tracking nil)
  (eat-enable-shell-prompt-annotation nil)
  (eat-term-scrollback-size nil)
  :config
  (evil-set-initial-state 'eat-mode 'emacs)
  :bind
  ("<key-chord> ü x" . eat))

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

(use-package wdired
  :defer t
  :config
  (evil-set-initial-state 'wdired-mode 'normal))

(provide 'nagy-evil)
;;; nagy-evil.el ends here
