;;; nagy-shell.el --- Shell and REPL config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") evil general nagy-use-package)

(require 'evil)
(require 'general)

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
  :bind
  ("s-<return>" . eshell))

(use-package shell
  :defer t
  :config
  (evil-set-initial-state 'shell-command-mode 'normal))
;; (evil-set-initial-state 'shell-command-mode 'emacs)

(use-package comint
  ;; :preface
  ;; (defun +nagy/comint-delete-clear ()
  ;;   (interactive)
  ;;   (comint-delete-input)
  ;;   (comint-clear-buffer))
  ;; (defun nagy-comint-kill-buffer-h ()
  ;;   (when (and (process-live-p (get-buffer-process (current-buffer)))
  ;;              (derived-mode-p 'comint-mode))
  ;;     (comint-interrupt-subjob)))
  ;; (add-hook 'kill-buffer-hook #'nagy-comint-kill-buffer-h)
  :hook
  (comint-mode-hook . visual-line-mode)
  :custom
  ;; can freeze emacs on something like sleep 10 if non-nil
  ;; https://old.reddit.com/r/emacs/comments/14377k9/weekly_tips_tricks_c_thread/jn8igpu/
  (comint-process-echoes nil)
  ;; (comint-move-point-for-output t)
  :bind
  (:map comint-mode-map
        ([remap revert-buffer-quick] . comint-clear-buffer)
        ("C-ö" . comint-next-input)
        ("H-Ö" . comint-previous-input)
        ("M-Ö" . comint-previous-input)
        ("H-x" . comint-send-eof)
        ("H-j" . comint-next-prompt)
        ("H-k" . comint-previous-prompt)
        ;; ("<insert-state> <key-chord> f j" . comint-send-input)
        ("<normal-state> <key-chord> f h" . embark-dwim)
        ("<normal-state> <key-chord> f j" . embark-act))

  :general
  (:states 'normal :keymaps 'comint-mode-map
           "Ö" #'comint-previous-input
           "ö" #'comint-send-input
           "_" #'comint-send-eof))

;; NIX-EMACS-PACKAGE: evil
(require 'evil)
(defun comint--evil-append ()
  (interactive)
  (pcase-let
      ((`(,lbeg . ,lend)
        (save-excursion
          (goto-char (point-max))
          (cons (line-beginning-position) (line-end-position)))))
    (unless (<= lbeg (point) lend)
      (goto-char (point-max))))
  (evil-append 0))
(evil-define-key 'normal comint-mode-map "a" #'comint--evil-append)

(defun comint--evil-insert ()
  (interactive)
  (pcase-let
      ((`(,lbeg . ,lend)
        (save-excursion
          (goto-char (point-max))
          (cons (line-beginning-position) (line-end-position)))))
    (unless (<= lbeg (point) lend)
      (goto-char (point-max))))
  (evil-insert 0))
(evil-define-key 'normal comint-mode-map "i" #'comint--evil-insert)

(use-package ielm
  :preface
  (defun ielm-on-buffer ()
    (interactive)
    (let* ((buf (current-buffer))
           (ielm-buf-name (concat "*ielm-" (buffer-name buf) "*")))
      (if-let* ((ielm-buf (get-buffer ielm-buf-name)))
          (switch-to-buffer ielm-buf)
        (ielm ielm-buf-name)
        (with-current-buffer ielm-buf-name
          (setq-local ielm-working-buffer buf)))))
  (defun nagy-shell-ielm-hook ()
    (setq mode-line-process '(":" (:eval (buffer-name ielm-working-buffer)))))
  ;; :config
  ;; (advice-add 'ielm-return :after #'evil-normal-state)
  :hook
  (inferior-emacs-lisp-mode-hook . nagy-shell-ielm-hook)
  :bind
  ("M-s-→" . ielm)
  (:map inferior-emacs-lisp-mode-map
        ("H-ö" . ielm-send-input)
        ;; ("H-b" . ielm-change-working-buffer)
        ("M-ö" . ielm-send-input)
        ("<key-chord> f j" . ielm-send-input)
        ("s-." . eval-last-sexp))
  :general
  (:states 'normal :keymaps 'inferior-emacs-lisp-mode-map
           "ö" #'ielm-return)
  :config
  (setq ielm-header "")) ; does not work in :custom because it is a defvar

(use-package sql
  :bind
  ("H-M-q" . sql-mode))

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
  :init
  (nagy-same-window "^\\*SQLite "))

(provide 'nagy-shell)
;;; nagy-shell.el ends here
