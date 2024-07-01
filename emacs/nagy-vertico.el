;;; nagy-vertico.el --- nagy-vertico config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") vertico embark consult consult-dir embark-consult marginalia orderless general nagy-evil nagy-use-package)

(require 'nagy-evil)

(require 'general)

(require 'consult)
(require 'embark)
(require 'embark-consult)

(use-package vertico
  :commands (vertico-mode)
  :config
  (vertico-mode)
  :custom
  (vertico-cycle nil)
  (vertico-scroll-margin most-positive-fixnum)
  (vertico-count 7)
  (vertico-resize nil)
  :bind
  ("C-s--" . vertico-flat-mode)
  (:map vertico-map
        ("C-j"   . vertico-next)
        ("C-M-j" . vertico-next-group)
        ("C-k"   . vertico-previous)
        ("C-M-k" . vertico-previous-group))
  (:map minibuffer-mode-map
        ("s--" . vertico-flat-mode)))

(use-package vertico-quick
  :custom
  (vertico-quick1 "asdfghjklö")
  (vertico-quick2 "vbnm")
  :bind
  (:map vertico-map
        ("<key-chord> f h" . vertico-quick-exit)
        ("<key-chord> f l" . vertico-quick-insert)
        ("C-," . vertico-quick-exit)
        ("C-." . vertico-quick-exit)))

(use-package vertico-buffer
  :custom
  (vertico-buffer-display-action '(display-buffer-same-window))
  :general
  (:states 'normal
           "°" #'vertico-buffer-mode))

(use-package consult
  :custom
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (consult-line-start-from-top t)
  :general
  (:states 'normal
           "s" #'consult-line
           "Ø" #'consult-outline)
  :bind
  ("s-b" . consult-buffer)
  ("s-/" . consult-focus-lines)
  ("H-/" . consult-keep-lines)
  ("H-s" . consult-line)
  ("C-s-s" . consult-ripgrep)
  ([remap bookmark-jump] . consult-bookmar)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap Info-search] . consult-info)
  ([remap load-theme] . consult-theme)
  ([remap man] . consult-man)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap recentf-open-files] . consult-recent-file)
  :config
  (keymap-global-set "<key-chord> ü j" #'consult-bookmark)
  (keymap-global-set "<key-chord> ü b" #'consult-buffer)
  (consult-customize
   consult-buffer
   :preview-key nil
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC"
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

(require 'orderless)
(orderless-define-completion-style orderless+initialism
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-regexp)))
(push '(command (styles orderless+initialism)) completion-category-overrides)
(push '(variable (styles orderless+initialism)) completion-category-overrides)
(push '(symbol (styles orderless+initialism)) completion-category-overrides)

(use-package marginalia
  :commands (marginalia-mode)
  :config
  (marginalia-mode))

(use-package consult-dir
  :bind
  ([remap list-directory] . consult-dir)
  (:map vertico-map
        ("C-x C-d" . consult-dir)
        ("C-x C-j" . consult-dir-jump-file)))

(provide 'nagy-vertico)
;;; nagy-vertico.el ends here
