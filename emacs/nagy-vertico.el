;;; nagy-dired.el --nagy-dired config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") vertico consult general nagy-use-package)

(require 'general)

(require 'consult)

(use-package vertico
  :custom
  (vertico-cycle nil)
  (vertico-scroll-margin most-positive-fixnum)
  (vertico-count 7))

(use-package vertico-quick
  :bind
  (:map vertico-map
        ("C-," . vertico-quick-exit)
        ("C-." . vertico-quick-exit)))

(use-package vertico-buffer
  :custom
  (vertico-buffer-display-action '(display-buffer-same-window))
  :general
  (:states 'normal
           "°" #'vertico-buffer-mode))

(use-package consult
  :general
  (:states 'normal "Ø" #'consult-outline)
  :bind
  ("H-b" . consult-buffer)
  ("s-/" . consult-focus-lines)
  ("H-/" . consult-keep-lines)
  :config
  (consult-customize
   consult-buffer :preview-key nil
   ;; Disable preview for `consult-theme' completely.
   consult-theme :preview-key nil))

(use-package consult-imenu
  :defer t
  :general
  (:states 'normal
           "ø" #'consult-imenu
           ;; "M-ø" #'imenu-list
           )
  :config
  (setq consult-imenu-config
        '((emacs-lisp-mode
           :toplevel "Section"
           :types ((?f "Functions"  font-lock-function-name-face)
                   (?m "Macros"    font-lock-function-name-face)
                   (?M "Major modes" font-lock-function-name-face)
                   (?p "Package"   font-lock-constant-face)
                   (?t "Types"     font-lock-type-face)
                   (?S "Section"   org-document-info)
                   (?v "Variables" font-lock-variable-name-face)))
          (lisp-mode
           :types ((?t "Types"      font-lock-type-face)
                   (?v "Variables"  font-lock-variable-name-face)))
          (magit-status-mode
           :toplevel "Staged changes"
           :types ((?s "Unstaged changes"  modus-themes-diff-removed)
                   (?s "Staged changes"  modus-themes-diff-added)
                   (?S "Stashes"        font-lock-function-name-face)
                   (?c "Recent commits"  modus-themes-diff-removed)))
          (magit-refs-mode
           :toplevel "Branches"
           :types ((?b "Branches"  modus-themes-diff-added)
                   (?t "Tags"     font-lock-function-name-face))))))

(provide 'nagy-vertico)
;;; nagy-vertico.el ends here
