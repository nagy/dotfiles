;;; nagy-window.el --- Window and frame config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package)

(require 'general)

(use-package tab-bar
  :config
  (tab-bar-mode)
  :custom
  (tab-bar-show 1)
  (tab-bar-auto-width t)
  ;; (tab-bar-auto-width-max '(330 30))
  ;; (tab-bar-auto-width-max '(440 40))
  ;; using this instead of 10000 below means no flickering in the tab bar
  (tab-bar-auto-width-max `(,(* 8 880) ,(* 8 80)))
  ;; (tab-bar-auto-width-max '(10000 1000))
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice t))

(use-package winner
  :custom
  ;; without this, in eat the C-C keys are incorrectly bound.
  (winner-dont-bind-my-keys t)          ; but does not seem to work
  :bind
  ("s-u" . winner-undo)
  ("s-U" . winner-redo))

(use-package repeat
  :bind
  ("M-s-." . repeat))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width 2)
  :bind
  ("M-ĸ" . display-line-numbers-mode)
  ("s-M-ĸ" . global-display-line-numbers-mode))

(use-package hl-line
  :bind
  ("M-ħ" . hl-line-mode))

;; NIX-EMACS-PACKAGE: olivetti
(use-package olivetti
  :custom
  (olivetti-body-width 120)
  :general
  (:states 'normal
           "“" #'olivetti-expand
           "”" #'olivetti-shrink))

;; NIX-EMACS-PACKAGE: ace-window
(use-package ace-window
  :custom
  (aw-keys (list ?a ?s ?d ?f ?h ?j ?k ?l))
  (aw-background nil)
  (aw-scope 'visible)
  (aw-leading-char-style 'path)
  :bind
  ("s-'" . ace-window))

;; NIX-EMACS-PACKAGE: focus
(use-package focus
  :preface
  (defun nagy/fix-focus-face ()
    (set-face-attribute 'focus-unfocused nil :foreground 'unspecified :inherit 'parenthesis))
  ;; (defun nagy/focus-tired-redraw ()
  ;;   ;; `tabulated-list-print' erases the listing, and the erase collapses
  ;;   ;; the three focus overlays, so a Tired print needs them back.
  ;;   (when (bound-and-true-p focus-mode)
  ;;     (focus-move-focus (current-buffer))))
  :init
  (setq focus-mode-to-thing
        '((prog-mode . defun)
          (markdown-ts-mode . paragraph)
          (elpher-mode . paragraph)
          (nix-mode . paragraph)
          (text-mode . sentence)))
          ;; The depth-zero entry of point with every row that it
          ;; prints below itself, however deep the cursor sits.
          ;; (tired-mode . tired-subtree)))
  :commands focus-mode
  :bind
  ("H-M-f" . focus-mode)
  :config
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/fix-focus-face))
  ;; (add-hook 'tired-after-redraw-hook #'nagy/focus-tired-redraw))

;; NIX-EMACS-PACKAGE: ws-butler
(use-package ws-butler
  :diminish ws-butler-mode
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :hook
  (prog-mode-hook . ws-butler-mode)
  (text-mode-hook . ws-butler-mode))

(provide 'nagy-window)
;;; nagy-window.el ends here
