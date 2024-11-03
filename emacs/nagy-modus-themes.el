;;; nagy-modus-themes.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: October 06, 2022
;; Modified: October 06, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") paren-face nerd-icons modus-themes ef-themes lin)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'modus-themes)
;; (require 'ef-themes)
(require 'paren-face)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'lin))

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
    (set-face-attribute 'tab-bar-tab-inactive nil :box nil :background (face-attribute 'tab-bar :background nil t))
    (set-face-attribute 'window-divider nil :foreground (if (dayp) "black" "gray20"))
    (with-eval-after-load 'dired
      (set-face-attribute 'dired-header nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)))
  (defun ala-fix-theme ()
    (interactive)
    (call-process (if (dayp) "ala-day" "ala-night")))
  :config
  (add-hook 'modus-themes-after-load-theme-hook #'nagy-modus-theme-overrides)
  (add-hook 'modus-themes-after-load-theme-hook #'ala-fix-theme)
  (load-theme 'modus-vivendi t))

(use-package modus-themes
  :preface
  (defun my-modus-themes-custom-faces ()
    (interactive)
    (modus-themes-with-colors
      (custom-set-faces
       `(default ((,c :height 100)))
       `(magit-section-heading ((,c :inherit modus-themes-heading-3)))
       `(fixed-pitch ((,c :height unspecified)))
       `(header-line ((,c :background unspecified)))
       `(eros-result-overlay-face ((,c :background unspecified :box (:line-width -1 :color ,fg-main))))
       `(modus-themes-lang-error ((,c :underline unspecified :inherit modus-themes-intense-red)))
       `(modus-themes-lang-warning ((,c :underline unspecified :inherit modus-themes-intense-yellow)))
       `(modus-themes-lang-note ((,c :underline unspecified :inherit modus-themes-intense-green)))
       `(line-number ((,c :foreground unspecified :background unspecified :inherit (parenthesis default))))
       `(line-number-current-line ((,c :foreground unspecified :background unspecified :inherit line-number)))
       ;; `(nix-search-version ((,c :inherit modus-themes-fg-cyan)))
       ;; `(tab-bar-tab-inactive ((,c :box nil :background ,bg-inactive)))
       `(nix-search-version ((,c :inherit marginalia-version)))
       `(nix-search-description ((,c :inherit marginalia-documentation)))
       `(mode-line ((,c :box unspecified)))
       `(mode-line-inactive ((,c :box unspecified)))
       `(nameless-face ((,c :inherit font-lock-comment-delimiter-face)))
       `(forge-pullreq-open ((,c :inherit modus-themes-fg-green)))
       `(forge-pullreq-merged ((,c :inherit modus-themes-fg-magenta)))
       `(eglot-highlight-symbol-face ((,c :underline t :bold t)))
       `(scroll-bar ((,c :box unspecified :background ,bg-main :foreground ,(if (dayp) "#ccc" "#333")))))))
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
  ;; :custom
  ;; (paren-face-modes '(js-mode))
  :preface
  (defun nagy/fix-parenface ()
    (set-face-attribute 'parenthesis nil :foreground (if (dayp) "#ccc" "#333")))
  :config
  (setq paren-face-regexp "[][(){};,]")
  (global-paren-face-mode 1)
  (nagy/fix-parenface)
  (push 'js-mode paren-face-modes)
  (push 'js-json-mode paren-face-modes)
  (push 'c-mode paren-face-modes)
  (push 'c++-mode paren-face-modes)
  (push 'nix-mode paren-face-modes)
  (push 'rustic-mode paren-face-modes)
  (push 'hy-mode paren-face-modes)
  (push 'groovy-mode paren-face-modes)
  (push 'terraform-mode paren-face-modes)
  (push 'jenkinsfile-mode paren-face-modes)
  (push 'nickel-mode paren-face-modes)
  (push 'python-mode paren-face-modes)
  (push 'python-ts-mode paren-face-modes)
  (push 'rust-ts-mode paren-face-modes)
  (push 'conf-toml-mode paren-face-modes)
  (push 'sql-mode paren-face-modes)
  (push 'zig-mode paren-face-modes)
  (push 'java-mode paren-face-modes)
  (push 'yaml-mode paren-face-modes)
  (push 'hcl-mode paren-face-modes)
  (push 'go-mode paren-face-modes)
  (push 'go-dot-mod-mode paren-face-modes)
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/fix-parenface))

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

(use-package nerd-icons
  :demand t
  :bind
  ("C-H-s-e" . nerd-icons-insert))

(provide 'nagy-modus-themes)
;;; nagy-modus-themes.el ends here
