;;; nagy-modus-themes.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;; NIX-EMACS-PACKAGE: modus-themes
(require 'modus-themes)

(defun dayp ()
  "Return non-nil if it is day (aka light theme)."
  (eq (frame-parameter nil 'background-mode) 'light))

;; Backwards compatibility for modus-themes 5 to modus-themes 4 code
(defface nagy-nuanced-green '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-nuanced-red '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-intense-blue '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-intense-cyan '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-intense-green '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-intense-red '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-intense-magenta '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-intense-yellow '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-subtle-blue '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-subtle-cyan '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-subtle-red '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-subtle-yellow '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-subtle-green '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-nuanced-cyan '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-fg-red-intense '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-fg-red-faint '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-fg-yellow-faint '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-fg-yellow-intense '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-fg-green-faint '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)
(defface nagy-fg-green-intense '((t (:foreground "black" :background "white"))) "A custom face." :group 'emacs)

(use-package modus-themes
  :demand t
  :preface
  (defun nagy-modus-theme-overrides ()
    (interactive)
    (setenv "GTK_THEME" (if (dayp) "" "Adwaita:dark"))
    ;; (set-face-attribute 'tab-bar-tab nil :inherit 'unspecified) ;; remove 'bold
    (with-eval-after-load 'org
      (set-face-attribute 'org-block nil :background 'unspecified)
      (set-face-attribute 'org-block-begin-line nil :background 'unspecified)
      (set-face-attribute 'org-block-end-line nil :background 'unspecified)
      )
    (with-eval-after-load 'treesit-fold
      (set-face-attribute 'treesit-fold-replacement-face nil :box 'unspecified)
      (set-face-attribute 'treesit-fold-replacement-mouse-face nil :box 'unspecified)
      )
    (set-face-attribute 'tab-bar-tab-inactive nil :box nil :background (face-attribute 'tab-bar :background nil t))
    (set-face-attribute 'window-divider nil :foreground (if (dayp) "black" "gray20"))
    (set-face-attribute 'scroll-bar nil :box 'unspecified :foreground (if (dayp) "#ccc" "#333"))
    )
  (defun ala-fix-theme ()
    (interactive)
    (cl-assert (zerop (call-process (if (dayp) "ala-day" "ala-night")))))
  (defun my-modus-themes-custom-faces ()
    (interactive)
    (modus-themes-with-colors
      (custom-set-faces
       ;; `(default ((,c :height 100)))
       `(dired-header ((,c :height unspecified :foundry unspecified)))
       `(magit-section-heading ((,c :inherit modus-themes-heading-3)))
       `(fixed-pitch ((,c :height unspecified)))
       `(header-line ((,c :background unspecified)))
       `(eros-result-overlay-face ((,c :background unspecified :box (:line-width -1 :color ,fg-main))))
       `(flymake-error ((,c :underline unspecified :inherit nagy-intense-red)))
       `(flymake-warning ((,c :underline unspecified :inherit nagy-intense-yellow)))
       `(flymake-note ((,c :underline unspecified :inherit nagy-intense-green)))
       `(line-number ((,c :foreground unspecified :background unspecified :inherit (parenthesis default))))
       `(line-number-current-line ((,c :foreground unspecified :background unspecified :inherit line-number)))
       `(nix-search-version ((,c :inherit marginalia-version)))
       `(nix-search-description ((,c :inherit marginalia-documentation)))
       `(nameless-face ((,c :inherit font-lock-comment-delimiter-face)))
       `(jinx-misspelled ((,c :inherit nagy-subtle-yellow)))
       `(eglot-highlight-symbol-face ((,c :underline t :bold t)))
       `(eglot-diagnostic-tag-unnecessary-face ((,c :underline unspecified :inherit nagy-intense-green)))
       `(scroll-bar ((,c :background ,bg-main)))
       ;; High contrast
       `(mode-line ((,c :box (:line-width 2))))
       `(mode-line-inactive ((,c :foreground ,fg-main :box (:line-width 2 :color ,bg-main) :background ,bg-main)))
       `(tab-bar-tab          ((,c :box (:line-width 2 :color ,fg-main))))
       `(tab-bar-tab-inactive ((,c :box (:line-width 2 :color ,bg-main) :background ,bg-main))) ;; use same-color box to fix "jumping"
       `(tab-bar ((,c :box nil :background ,bg-main)))
       `(tab-line ((,c :box nil :background ,bg-main)))
       )))
  (defun my-modus-themes-custom-faces2 ()
    ;; BG
    (let ((lst '((nuanced-green . green-nuanced)
                 (nuanced-red . red-nuanced)
                 (intense-blue . blue-intense)
                 (intense-cyan . cyan-intense)
                 (intense-green . green-intense)
                 (intense-red . red-intense)
                 (intense-magenta . magenta-intense)
                 (intense-yellow . yellow-intense)
                 (subtle-blue . blue-subtle)
                 (subtle-cyan . cyan-subtle)
                 (subtle-red . red-subtle)
                 (subtle-yellow . yellow-subtle)
                 (subtle-green . green-subtle)
                 (nuanced-cyan . cyan-nuanced))))
      (pcase-dolist (`(,name . ,other) lst)
        (set-face-attribute (intern (concat "nagy-" (symbol-name name))) nil
                            :foreground (modus-themes-get-color-value 'fg-main)
                            :background (modus-themes-get-color-value (intern (concat "bg-" (symbol-name other)))))))
    ;; FG
    (let ((lst '((red-intense . red-intense)
                 (red-faint . red-faint)
                 (yellow-intense . yellow-intense)
                 (yellow-faint . yellow-faint)
                 (green-intense . green-intense)
                 (green-faint . green-faint))))
      (pcase-dolist (`(,name . ,other) lst)
        (set-face-attribute (intern (concat "nagy-fg-" (symbol-name name))) nil
                            :foreground (modus-themes-get-color-value (intern (symbol-name other)))
                            :background (modus-themes-get-color-value 'bg-main))))
    )
  :bind
  ("H-<f2>" . modus-themes-toggle)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)   ; kursiv
  ;; (modus-themes-org-blocks 'tinted-background)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-completions '((matches . (background))
                              (selection . (semibold accented intense))
                              (popup . (accented))))
  (modus-themes-common-palette-overrides '((bg-mode-line-active bg-main)
                                           (bg-mode-line-inactive bg-inactive)
                                           (bg-region bg-blue-subtle)
                                           (fg-region unspecified)
                                           ;; (bg-tab-bar bg-main)
                                           ;; (bg-tab-current bg-active)
                                           ;; (bg-tab-other bg-inactive)
                                           ))
  :hook
  (modus-themes-after-load-theme . nagy-modus-theme-overrides)
  (modus-themes-after-load-theme . ala-fix-theme)
  (modus-themes-after-load-theme . my-modus-themes-custom-faces)
  (modus-themes-after-load-theme . my-modus-themes-custom-faces2)
  :config
  ;; (add-hook 'modus-themes-after-load-theme-hook #'nagy-modus-theme-overrides)
  ;; (add-hook 'modus-themes-after-load-theme-hook #'ala-fix-theme)
  ;; (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces2)
  (modus-themes-load-theme 'modus-vivendi)
  )

;; NIX-EMACS-PACKAGE: paren-face
(use-package paren-face
  :demand t
  :custom
  ;; (paren-face-modes '(...))
  (paren-face-regexp "[][(){};,]")
  :preface
  (defun nagy--fix-paren-face ()
    (interactive)
    (set-face-attribute 'parenthesis nil :foreground (if (dayp) "#ccc" "#333")))
  :config
  (global-paren-face-mode 1)
  (nagy--fix-paren-face)
  (add-to-list 'paren-face-modes 'js-mode)
  (add-to-list 'paren-face-modes 'typescript-mode)
  (add-to-list 'paren-face-modes 'js-json-mode)
  (add-to-list 'paren-face-modes 'c-mode)
  (add-to-list 'paren-face-modes 'c++-mode)
  (add-to-list 'paren-face-modes 'nix-mode)
  (add-to-list 'paren-face-modes 'rustic-mode)
  (add-to-list 'paren-face-modes 'hy-mode)
  (add-to-list 'paren-face-modes 'nickel-mode)
  (add-to-list 'paren-face-modes 'python-mode)
  (add-to-list 'paren-face-modes 'python-ts-mode)
  (add-to-list 'paren-face-modes 'rust-ts-mode)
  (add-to-list 'paren-face-modes 'conf-toml-mode)
  (add-to-list 'paren-face-modes 'sql-mode)
  (add-to-list 'paren-face-modes 'zig-mode)
  (add-to-list 'paren-face-modes 'java-mode)
  (add-to-list 'paren-face-modes 'yaml-mode)
  (add-to-list 'paren-face-modes 'hcl-mode)
  (add-to-list 'paren-face-modes 'go-mode)
  (add-to-list 'paren-face-modes 'go-dot-mod-mode)
  (add-to-list 'paren-face-modes 'lua-mode)
  (add-to-list 'paren-face-modes 'fennel-mode)
  ;; (add-hook 'modus-themes-after-load-theme-hook #'nagy--fix-paren-face 100) ;; needs to be at the end
  :hook
  (modus-themes-after-load-theme . nagy--fix-paren-face)
  )

(provide 'nagy-modus-themes)
;;; nagy-modus-themes.el ends here
