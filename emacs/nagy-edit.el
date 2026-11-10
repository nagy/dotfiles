;;; nagy-edit.el --- Editing convenience config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package)

(require 'general)

(use-package abbrev
  :defines (nix-repl-mode-abbrev-table inferior-emacs-lisp-mode-abbrev-table)
  :diminish abbrev-mode
  :hook
  (prog-mode-hook . abbrev-mode)
  (comint-mode-hook . abbrev-mode)
  (text-mode-hook . abbrev-mode)
  :config
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

;; NIX-EMACS-PACKAGE: super-save
;; TODO carry over doom autoload
(use-package super-save
  :preface
  (defun nagy-edit-super-save-predicate ()
    (string-prefix-p temporary-file-directory default-directory))
  :demand t
  :diminish super-save-mode
  :commands (super-save-mode)
  ;; :custom
  ;; (super-save-auto-save-when-idle t)
  :config
  (add-to-list 'super-save-predicates #'nagy-edit-super-save-predicate 'append)
  (add-to-list 'super-save-triggers 'previous-window-any-frame)
  (add-to-list 'super-save-triggers 'next-window-any-frame)
  (add-to-list 'super-save-triggers 'delete-window-or-tab)
  (add-to-list 'super-save-triggers 'silent-tab-next)
  (add-to-list 'super-save-triggers 'silent-tab-previous)
  (super-save-mode 1))

;; NIX-EMACS-PACKAGE: beginend
(use-package beginend
  ;; :diminish beginend-global-mode
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode))
  (beginend-global-mode)
  :bind
  ("H-<up>" . beginning-of-buffer)
  ("H-<down>" . end-of-buffer))

(use-package aggressive-indent
  :diminish 'aggressive-indent-mode
  :custom
  (aggressive-indent-sit-for-time 0.5)
  :general
  (:states 'normal :keymaps 'prog-mode-map
           "«" #'aggressive-indent-mode))

;; NIX-EMACS-PACKAGE: eros
(use-package eros
  :commands (eros-mode)
  :custom
  (eros-eval-result-prefix "")
  :config
  (eros-mode 1))

;; NIX-EMACS-PACKAGE: obvious
(declare-function obvious-mode "obvious")

(use-package obvious
  :commands (nagy-obvious-mode)
  :defer t
  :custom
  (obvious-headers nil)
  ;; (obvious-preserve-blank-lines nil)
  :preface
  (defun nagy-obvious-mode (&optional arg)
    (interactive
     (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg)
             'toggle)))
    (read-only-mode arg)
    (obvious-mode arg))
  :bind
  ("<f12>" . nagy-obvious-mode)
  ("H-s-w" . nagy-obvious-mode))

;; NIX-EMACS-PACKAGE: drag-stuff
(use-package drag-stuff
  :defer t
  :bind
  ("M-k" . drag-stuff-up)
  ("M-j" . drag-stuff-down))
;; ("<M-left>"  . drag-stuff-left)
;; ("<M-right>" . drag-stuff-right)
;; :config
;; (define-key esc-map "j" nil)

(provide 'nagy-edit)
;;; nagy-edit.el ends here
