;;; nagy-project.el --- Project and buffer config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package)

(require 'general)

(use-package project
  :preface
  (defun nagy-project-find-file-flake-nix ()
    (interactive)
    (when (project-root (project-current))
      (find-file (concat (project-root (project-current)) "/flake.nix"))))
  (defun nagy-project-find-file-dot-git-config ()
    (interactive)
    (when (project-root (project-current))
      (find-file (concat (project-root (project-current)) "/.git/config"))))
  ;; :custom
  ;; (project-mode-line t)
  ;; (project-list-exclude nil)
  :config
  (keymap-global-set "H-p" project-prefix-map)
  (keymap-set project-prefix-map "s-k" #'project-kill-buffers)
  (keymap-set project-prefix-map "s-j" #'project-dired)
  (keymap-set project-prefix-map "1" #'nagy-project-find-file-flake-nix)
  (keymap-set project-prefix-map "2" #'nagy-project-find-file-dot-git-config)
  (keymap-global-set "s-," #'project-dired)
  :bind
  ("C-s-<return>" . project-eshell)
  ("C-H-s--" . project-dired))

(use-package xref
  :defer t
  :custom
  (xref-search-program 'ripgrep))

(use-package ibuffer
  :after evil-collection ;; to make the bindings below work
  :preface
  (defun nagy-project--ibuffer-cd-tmp ()
    (cd temporary-file-directory))
  :custom
  (ibuffer-expert t)
  ;; (ibuffer-human-readable-size t)       ;; emacs 31
  (ibuffer-display-summary nil)
  (ibuffer-formats '((mark modified read-only locked " "
                           (name 42 42 :left :elide)
                           " "
                           (size 9 -1 :right)
                           " "
                           (mode 16 16 :left :elide)
                           " " filename-and-process)
                     (mark " "
                           (name 16 -1)
                           " " filename)))
  ;; :config
  ;; TODO make this work:
  ;; (evil-define-key 'normal ibuffer-mode-map "," nil)
  :hook
  (ibuffer-mode-hook . nagy-project--ibuffer-cd-tmp))

;; NIX-EMACS-PACKAGE: tablist
(use-package tablist
  :bind
  (:map tablist-mode-map
        ("M-r" . tablist-pop-filter)
        ("M-/" . tablist-push-regexp-filter)))

(declare-function magit-current-section "magit-section")
(declare-function magit-region-sections "magit-section")

;; NIX-EMACS-PACKAGE: bufler
(use-package bufler
  :defer t
  :commands (bufler-define-buffer-command bufler--map-sections)
  :custom
  ;; Remove buffer mode annotation
  (bufler-buffer-mode-annotate-preds nil)
  :config
  ;; Remove bufler column "VC". might be expensive
  (setq bufler-columns (remove "VC" bufler-columns))
  (bufler-define-buffer-command simple-switch "Simple switcher"
    #'switch-to-buffer :refresh-p nil))

;; NIX-EMACS-PACKAGE: breadcrumb
(use-package breadcrumb
  :bind
  ("C-H-j" . breadcrumb-jump)
  :general
  (:states 'normal
           "¿" #'breadcrumb-mode))

(provide 'nagy-project)
;;; nagy-project.el ends here
