;;; nagy-modus-themes.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;; NIX-EMACS-PACKAGE: modus-themes
(require 'modus-themes)
;; NIX-EMACS-PACKAGE: paren-face
(require 'paren-face)

;; NIX-EMACS-PACKAGE: ef-themes
;; (require 'ef-themes)
;; NIX-EMACS-PACKAGE: doric-themes
;; (require 'doric-themes)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'lin))

;; Backwards compatibility for modus-themes 5 to modus-themes 4 code
(defface nagy-nuanced-green '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-nuanced-red '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-intense-blue '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-intense-cyan '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-intense-green '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-intense-red '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-intense-magenta '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-intense-yellow '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-subtle-blue '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-subtle-cyan '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-subtle-red '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-subtle-yellow '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-subtle-green '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-nuanced-cyan '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-fg-red-intense '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-fg-red-faint '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-fg-yellow-faint '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-fg-yellow-intense '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-fg-green-faint '((t (:foreground "black" :background "white"))) "A custom face.")
(defface nagy-fg-green-intense '((t (:foreground "black" :background "white"))) "A custom face.")
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
                          :foreground (modus-themes-get-color-value 'bg-main)
                          :background (modus-themes-get-color-value (intern (symbol-name other))))))
  )
(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces2)

(use-package modus-themes
  :preface
  (defun dayp ()
    "Return non-nil if it is day (aka light theme)."
    (eq (frame-parameter nil 'background-mode) 'light))
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)   ; kursiv
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-completions
   '((matches . (background))
     (selection . (semibold accented intense))
     (popup . (accented))))
  (modus-themes-common-palette-overrides
   '((bg-mode-line-active bg-main)
     (bg-mode-line-inactive bg-inactive)
     (bg-region bg-blue-subtle)
     (fg-region unspecified)
     ;; (bg-tab-bar bg-main)
     ;; (bg-tab-current bg-active)
     ;; (bg-tab-other bg-inactive)
     )))

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
    (with-eval-after-load 'dired
      (set-face-attribute 'dired-header nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)))
  (defun ala-fix-theme ()
    (interactive)
    (cl-assert (zerop (call-process (if (dayp) "ala-day" "ala-night")))))
  :config
  (add-hook 'modus-themes-after-load-theme-hook #'nagy-modus-theme-overrides)
  (add-hook 'modus-themes-after-load-theme-hook #'ala-fix-theme)
  (load-theme 'modus-vivendi t))

(use-package modus-themes
  :preface
  (defun my-modus-themes-custom-faces ()
    (interactive)
    (let (;; (c '((class color) (min-colors 256)))
          )
      (modus-themes-with-colors
        (custom-set-faces
         `(default ((,c :height 100)))
         `(magit-section-heading ((,c :inherit modus-themes-heading-3)))
         `(fixed-pitch ((,c :height unspecified)))
         `(header-line ((,c :background unspecified)))
         `(eros-result-overlay-face ((,c :background unspecified :box (:line-width -1 :color ,fg-main))))
         `(flymake-error ((,c :underline unspecified :inherit nagy-intense-red)))
         `(flymake-warning ((,c :underline unspecified :inherit nagy-intense-yellow)))
         `(flymake-note ((,c :underline unspecified :inherit nagy-intense-green)))
         `(line-number ((,c :foreground unspecified :background unspecified :inherit (parenthesis default))))
         `(line-number-current-line ((,c :foreground unspecified :background unspecified :inherit line-number)))
         ;; `(nix-search-version ((,c :inherit nagy-fg-cyan)))
         ;; `(tab-bar-tab-inactive ((,c :box nil :background ,bg-inactive)))
         `(nix-search-version ((,c :inherit marginalia-version)))
         `(nix-search-description ((,c :inherit marginalia-documentation)))
         `(mode-line ((,c :box unspecified)))
         `(mode-line-active ((,c :box (:line-width 2))))
         `(mode-line-inactive ((,c :box unspecified)))
         `(nameless-face ((,c :inherit font-lock-comment-delimiter-face)))
         ;; `(forge-pullreq-open ((,c :inherit nagy-fg-green)))
         ;; `(forge-pullreq-merged ((,c :inherit nagy-fg-magenta)))
         `(eglot-highlight-symbol-face ((,c :underline t :bold t)))
         `(eglot-diagnostic-tag-unnecessary-face ((,c :underline unspecified :inherit nagy-intense-green)))
         `(scroll-bar ((,c :box unspecified :background ,bg-main :foreground ,(if (dayp) "#ccc" "#333"))))
         ;; High contrast
         `(mode-line ((,c :box (:line-width 2))))
         `(mode-line-inactive ((,c :box (:line-width 2 ))))
         `(mode-line-inactive ((,c :foreground ,fg-main :box (:line-width 2 :color ,bg-main) :background ,bg-main)))
         `(tab-bar-tab          ((,c :box (:line-width 2 :color ,fg-main))))
         `(tab-bar-tab-inactive ((,c :box (:line-width 2 :color ,bg-main) :background ,bg-main))) ;; use same-color box to fix "jumping"
         `(tab-bar ((,c :box nil :background ,bg-main)))
         ))))
  (defun my-modus-themes-custom-faces-twice ()
    "Call `my-modus-themes-custom-faces' twice because with only one
call, the scrollbar on the second display is not updated
correctly."
    (interactive)
    (my-modus-themes-custom-faces)
    (my-modus-themes-custom-faces))
  :config
  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces-twice)
  ;; (add-hook 'modus-themes-after-load-theme-hook #'ala-fix-theme)
  )


(use-package paren-face
  :custom
  ;; (paren-face-modes '(js-mode))
  (paren-face-regexp "[][(){};,]")
  :preface
  (defun nagy/fix-parenface ()
    (set-face-attribute 'parenthesis nil :foreground (if (dayp) "#ccc" "#333")))
  :config
  (global-paren-face-mode 1)
  (nagy/fix-parenface)
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
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/fix-parenface))

;; NIX-EMACS-PACKAGE: lin
(use-package lin
  :functions lin-global-mode
  ;; :custom
  ;; (lin-mode-hooks '(elfeed-search-mode-hook))
  :config
  (setq lin-mode-hooks
        '(elfeed-search-mode-hook
          ibuffer-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          nix-search-mode-hook
          proced-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode 1)
  (set-face-attribute 'hl-line nil :inherit 'lin-blue :background 'unspecified))

;; NIX-EMACS-PACKAGE: nerd-icons
(use-package nerd-icons
  :demand t
  :bind
  ("C-H-s-e" . nerd-icons-insert))

(provide 'nagy-modus-themes)
;;; nagy-modus-themes.el ends here
