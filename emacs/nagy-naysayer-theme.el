;;; nagy-naysayer-theme.el --- Naysayer theme configuration -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-modus-themes)

;; Optional dark theme (inspired by Jonathan Blow's compiler livestreams).
;; Not enabled by default — modus remains the active theme. Try it with
;; `M-x load-theme RET naysayer' or H-<f3>.
;; Face tweaks mirror the modus-themes block in nagy-modus-themes.el and are
;; applied from `enable-theme-functions' whenever the naysayer theme enables.

(require 'nagy-modus-themes) ; for `ala-fix-theme' and `dayp'

;; NIX-EMACS-PACKAGE: naysayer-theme
(use-package naysayer-theme
  :demand t
  :preface
  (defconst nagy-naysayer-palette
    '((bg-main . "#062329")        ; page background
      (bg-alt  . "#0b3335")        ; panels / highlight-line
      (bg-hover . "#0d3a3d")       ; hover
      (bg-border . "#126367")      ; line-fg / borders
      (bg-inactive . "#041a1e")    ; inset
      (bg-region . "#0000ff")      ; selection
      (fg-main . "#d1b897")        ; text (tan)
      (fg-alt  . "#c1d1e3")        ; variables/methods
      (fg-faint . "#7ad0c6")       ; constants
      (fg-comment . "#44b340")     ; comments
      (fg-string . "#2ec09c")      ; strings
      (fg-macro . "#8cde94")       ; macros/punctuation
      (fg-keyword . "#ffffff")     ; keywords
      (line-fg . "#4a8b91")        ; line numbers
      (line-fg-current . "#f0e8da")
      (blue . "#66d9ef")           ; monokai accents
      (violet . "#ae81ff")
      (green . "#a6e22e")
      (yellow . "#e6db74")
      (orange . "#fd971f")
      (red . "#f92672")
      (magenta . "#fd5ff0")
      (cyan . "#a1efe4")
      ;; tinted backgrounds (dark ramp steps, light text safe)
      (bg-green-nuanced . "#142005") (bg-red-nuanced . "#370516") (bg-cyan-nuanced . "#0a2835")
      (bg-green-subtle . "#1c2d07") (bg-red-subtle . "#4b0720") (bg-cyan-subtle . "#0e3445")
      (bg-blue-subtle . "#0e3445") (bg-yellow-subtle . "#402204")
      (bg-green-intense . "#38570e") (bg-red-intense . "#6a0a2d") (bg-cyan-intense . "#1c5d70")
      (bg-blue-intense . "#14475a") (bg-yellow-intense . "#7a4309") (bg-magenta-intense . "#3a2466")
      ;; foreground-only
      (fg-red-intense . "#f92672") (fg-red-faint . "#ab1248")
      (fg-yellow-intense . "#e6db74") (fg-yellow-faint . "#9a560c")
      (fg-green-intense . "#a6e22e") (fg-green-faint . "#4b7413"))
    "Naysayer palette, mirroring the modus-themes color names used in
nagy-modus-themes.el.")

  (defun nagy-load-naysayer-theme ()
    "Load the naysayer theme, replacing the active modus theme."
    (interactive)
    (load-theme 'naysayer t))

  (defun nagy-naysayer--theme-overrides ()
    "Environment and misc tweaks for the naysayer (dark) theme."
    (setenv "GTK_THEME" "Adwaita:dark")
    (with-eval-after-load 'org
      (set-face-attribute 'org-block nil :background (alist-get 'bg-alt nagy-naysayer-palette))
      (set-face-attribute 'org-code nil :foreground "#ffb454" :background (alist-get 'bg-alt nagy-naysayer-palette))
      (set-face-attribute 'org-block-begin-line nil :background 'unspecified)
      (set-face-attribute 'org-block-end-line nil :background 'unspecified))
    (with-eval-after-load 'treesit-fold
      (set-face-attribute 'treesit-fold-replacement-face nil :box 'unspecified)
      (set-face-attribute 'treesit-fold-replacement-mouse-face nil :box 'unspecified))
    (with-eval-after-load 'parinfer-rust-mode
      (set-face-attribute 'parinfer-rust-dim-parens nil :foreground 'unspecified :inherit '(parenthesis)))
    (set-face-attribute 'tab-bar-tab-inactive nil :box nil :background (face-attribute 'tab-bar :background nil t))
    (set-face-attribute 'window-divider nil :foreground "gray20"))

  (defun nagy-naysayer--init-derived-faces ()
    "Set colors on `nagy-*' faces from the naysayer palette."
    (let ((bg-faces '((nagy-nuanced-green . bg-green-nuanced)
                      (nagy-nuanced-red . bg-red-nuanced)
                      (nagy-nuanced-cyan . bg-cyan-nuanced)
                      (nagy-intense-blue . bg-blue-intense)
                      (nagy-intense-cyan . bg-cyan-intense)
                      (nagy-intense-green . bg-green-intense)
                      (nagy-intense-red . bg-red-intense)
                      (nagy-intense-magenta . bg-magenta-intense)
                      (nagy-intense-yellow . bg-yellow-intense)
                      (nagy-subtle-blue . bg-blue-subtle)
                      (nagy-subtle-cyan . bg-cyan-subtle)
                      (nagy-subtle-red . bg-red-subtle)
                      (nagy-subtle-yellow . bg-yellow-subtle)
                      (nagy-subtle-green . bg-green-subtle)))
          (fg-faces '((nagy-fg-red-intense . fg-red-intense)
                      (nagy-fg-red-faint . fg-red-faint)
                      (nagy-fg-yellow-intense . fg-yellow-intense)
                      (nagy-fg-yellow-faint . fg-yellow-faint)
                      (nagy-fg-green-intense . fg-green-intense)
                      (nagy-fg-green-faint . fg-green-faint))))
      (pcase-dolist (`(,face . ,key) bg-faces)
        (set-face-attribute face nil
                            :foreground (alist-get 'fg-main nagy-naysayer-palette)
                            :background (alist-get key nagy-naysayer-palette)))
      (pcase-dolist (`(,face . ,key) fg-faces)
        (set-face-attribute face nil
                            :foreground (alist-get key nagy-naysayer-palette)
                            :background (alist-get 'bg-main nagy-naysayer-palette)))))

  (defun nagy-naysayer--custom-faces ()
    "Apply naysayer face tweaks, mirroring `nagy-modus-themes--custom-faces'."
    (let ((bg-main (alist-get 'bg-main nagy-naysayer-palette))
          (bg-alt (alist-get 'bg-alt nagy-naysayer-palette))
          (bg-hover (alist-get 'bg-hover nagy-naysayer-palette))
          (fg-main (alist-get 'fg-main nagy-naysayer-palette))
          (fg-alt (alist-get 'fg-alt nagy-naysayer-palette))
          (fg-faint (alist-get 'fg-faint nagy-naysayer-palette))
          (fg-macro (alist-get 'fg-macro nagy-naysayer-palette))
          (line-fg (alist-get 'line-fg nagy-naysayer-palette))
          (line-fg-current (alist-get 'line-fg-current nagy-naysayer-palette))
          (blue (alist-get 'blue nagy-naysayer-palette))
          (yellow (alist-get 'yellow nagy-naysayer-palette))
          (fg-green-intense (alist-get 'fg-green-intense nagy-naysayer-palette))
          (bg-green-nuanced (alist-get 'bg-green-nuanced nagy-naysayer-palette))
          (bg-red-nuanced (alist-get 'bg-red-nuanced nagy-naysayer-palette))
          (bg-blue-subtle (alist-get 'bg-blue-subtle nagy-naysayer-palette))
          (bg-yellow-subtle (alist-get 'bg-yellow-subtle nagy-naysayer-palette))
          (bg-green-intense (alist-get 'bg-green-intense nagy-naysayer-palette))
          (bg-red-intense (alist-get 'bg-red-intense nagy-naysayer-palette))
          (bg-blue-intense (alist-get 'bg-blue-intense nagy-naysayer-palette))
          (bg-cyan-intense (alist-get 'bg-cyan-intense nagy-naysayer-palette))
          (bg-yellow-intense (alist-get 'bg-yellow-intense nagy-naysayer-palette))
          (red (alist-get 'red nagy-naysayer-palette))
          (border (alist-get 'bg-border nagy-naysayer-palette))) ; #126367, same as the docs.rs scrollbar thumb
      (custom-set-faces
       `(dired-header ((t :height unspecified :foundry unspecified)))
       `(magit-section-heading ((t :inherit font-lock-keyword-face)))
       `(fixed-pitch ((t :height unspecified)))
       `(header-line ((t :background unspecified)))
       `(eros-result-overlay-face ((t :background unspecified :box (:line-width -1 :color ,fg-main))))
       `(error ((t :foreground ,red :weight normal)))
       `(warning ((t :foreground ,yellow :weight normal)))
       `(success ((t :foreground ,fg-green-intense :weight normal)))
       `(tooltip ((t :foreground ,fg-main :background ,bg-alt)))
       `(help-key-binding ((t :foreground ,fg-faint :background ,bg-alt :inherit fixed-pitch :box (:line-width -1 :color ,border))))
       `(minibuffer-prompt ((t :foreground ,fg-faint)))
       `(secondary-selection ((t :foreground unspecified :background ,bg-blue-subtle :extend t)))
       `(highlight ((t :foreground unspecified :background ,bg-blue-intense :extend t)))
       `(completions-common-part ((t :foreground ,blue)))
       `(completions-highlight ((t :foreground unspecified :background ,bg-blue-intense)))
       ;; search
       `(isearch ((t :foreground ,line-fg-current :background ,bg-blue-intense :weight bold)))
       `(isearch-fail ((t :foreground ,fg-main :background ,bg-red-intense)))
       `(isearch-group-1 ((t :foreground ,line-fg-current :background ,bg-blue-intense)))
       `(isearch-group-2 ((t :foreground ,line-fg-current :background ,bg-cyan-intense)))
       `(lazy-highlight ((t :foreground unspecified :background ,bg-hover :extend t)))
       `(match ((t :foreground unspecified :background ,bg-cyan-intense :extend t)))
       ;; tree-sitter punctuation and literals (Emacs 31 face names)
       `(font-lock-number-face ((t :foreground ,fg-faint)))
       `(font-lock-operator-face ((t :foreground ,fg-macro)))
       `(font-lock-punctuation-face ((t :foreground ,fg-macro)))
       `(font-lock-misc-punctuation-face ((t :foreground ,fg-macro)))
       `(font-lock-bracket-face ((t :foreground ,fg-macro)))
       `(font-lock-delimiter-face ((t :foreground ,fg-macro)))
       `(font-lock-property-name-face ((t :foreground ,fg-alt)))
       `(font-lock-property-use-face ((t :foreground ,fg-alt)))
       ;; diff and ediff
       `(diff-header ((t :foreground ,fg-main :background ,bg-alt :extend t)))
       `(diff-hunk-header ((t :foreground ,fg-main :background ,bg-blue-subtle :extend t)))
       `(diff-added ((t :foreground unspecified :background ,bg-green-nuanced :extend t)))
       `(diff-removed ((t :foreground unspecified :background ,bg-red-nuanced :extend t)))
       `(diff-changed ((t :foreground unspecified :background ,bg-yellow-subtle :extend t)))
       `(diff-refine-added ((t :foreground unspecified :background ,bg-green-intense)))
       `(diff-refine-removed ((t :foreground unspecified :background ,bg-red-intense)))
       `(diff-refine-changed ((t :foreground unspecified :background ,bg-yellow-intense)))
       `(ediff-current-diff-A ((t :foreground unspecified :background ,bg-red-nuanced :extend t)))
       `(ediff-current-diff-B ((t :foreground unspecified :background ,bg-green-nuanced :extend t)))
       `(ediff-current-diff-C ((t :foreground unspecified :background ,bg-yellow-subtle :extend t)))
       `(ediff-fine-diff-A ((t :foreground unspecified :background ,bg-red-intense :extend t)))
       `(ediff-fine-diff-B ((t :foreground unspecified :background ,bg-green-intense :extend t)))
       `(ediff-fine-diff-C ((t :foreground unspecified :background ,bg-yellow-intense :extend t)))
       `(ediff-even-diff-A ((t :foreground unspecified :background ,bg-alt :extend t)))
       `(ediff-even-diff-B ((t :foreground unspecified :background ,bg-alt :extend t)))
       `(ediff-even-diff-C ((t :foreground unspecified :background ,bg-alt :extend t)))
       `(ediff-odd-diff-A ((t :foreground unspecified :background ,bg-hover :extend t)))
       `(ediff-odd-diff-B ((t :foreground unspecified :background ,bg-hover :extend t)))
       `(ediff-odd-diff-C ((t :foreground unspecified :background ,bg-hover :extend t)))
       `(log-view-message ((t :foreground ,fg-main :background ,bg-alt :extend t)))
       `(log-view-file ((t :foreground ,fg-main :background ,bg-blue-subtle :weight bold :extend t)))
       `(compilation-error ((t :foreground ,red :weight normal)))
       `(compilation-mode-line-fail ((t :foreground ,red :weight bold)))
       `(trailing-whitespace ((t :foreground unspecified :background ,bg-red-intense)))
       `(flymake-error ((t :underline unspecified :inherit nagy-intense-red)))
       `(flymake-warning ((t :underline unspecified :inherit nagy-intense-yellow)))
       `(flymake-note ((t :underline unspecified :inherit nagy-intense-green)))
       `(line-number ((t :foreground ,line-fg :background unspecified)))
       `(line-number-current-line ((t :foreground ,line-fg-current :background unspecified)))
       `(nix-search-version ((t :inherit marginalia-version)))
       `(nix-search-description ((t :inherit marginalia-documentation)))
       `(nameless-face ((t :inherit font-lock-comment-delimiter-face)))
       `(jinx-misspelled ((t :inherit nagy-subtle-yellow)))
       `(eglot-highlight-symbol-face ((t :underline t :bold t)))
       `(eglot-diagnostic-tag-unnecessary-face ((t :underline unspecified :inherit nagy-intense-green)))
       `(org-modern-horizontal-rule ((t :underline ,border)))
       `(scroll-bar ((t :background ,bg-main :foreground ,border))) ; fg here so custom-set-faces does not reset it
       `(margin ((t :background ,bg-main)))
       ;; High contrast
       `(mode-line ((t :foreground ,fg-main :background ,bg-main :box (:line-width 2)))) ; same bg as the inactive mode-line
       `(mode-line-active ((t :foreground ,fg-main :background ,bg-main :box (:line-width 2)))) ; Emacs 29+ real face
       `(mode-line-inactive ((t :foreground ,fg-main :box (:line-width 2 :color ,bg-main) :background ,bg-main)))
       `(tab-bar-tab          ((t :foreground ,fg-main :background ,bg-main :box (:line-width 2 :color ,fg-main)))) ; readable fg, no "text" bg
       `(tab-bar-tab-inactive ((t :box (:line-width 2 :color ,bg-main) :background ,bg-main))) ;; same-color box to fix "jumping"
       `(tab-bar ((t :box nil :background ,bg-main)))
       `(tab-line-tab          ((t :foreground ,fg-main :background ,bg-main :box (:line-width 2 :color ,fg-main)))) ; readable fg, no "text" bg
       `(tab-line-tab-current  ((t :foreground ,fg-main :background ,bg-main :box (:line-width 2 :color ,fg-main))))
       `(tab-line-tab-inactive ((t :background ,bg-main :box (:line-width 2 :color ,bg-main)))) ;; same-color box to fix "jumping"
       `(tab-line ((t :box nil :background ,bg-main))))))

  (defun nagy-naysayer--on-theme-change (theme)
    "Apply naysayer customizations after the naysayer theme is enabled."
    (when (eq theme 'naysayer)
      (nagy-naysayer--theme-overrides)
      (ala-fix-theme)
      (nagy-naysayer--custom-faces)
      (nagy-naysayer--init-derived-faces)))
  :bind
  ("H-<f3>" . nagy-load-naysayer-theme)
  :config
  (add-hook 'enable-theme-functions #'nagy-naysayer--on-theme-change))

(provide 'nagy-naysayer-theme)
;;; nagy-naysayer-theme.el ends here
