;;; nagy-dired.el --nagy-dired config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") ov general dired-narrow dired-subtree)

(require 'dired)
(require 'ov)
(require 'general)

(defun nagy-dired-flake-underline ()
  (interactive)
  (ov-set "flake\\.nix" 'face '(:underline t))
  (ov-set "default\\.nix" 'face '(:underline t))
  ;; uninteresting, may also be moved to dired-omit mode
  (ov-set "flake\\.lock$" 'face 'parenthesis)
  (ov-set "Cargo\\.lock$" 'face 'parenthesis)
  (ov-set "Gemfile\\.lock$" 'face 'parenthesis)
  (ov-set "\\.dir-locals\\.el$" 'face 'parenthesis)
  (ov-set "\\.gitignore$" 'face 'parenthesis)
  (ov-set "\\.dockerignore$" 'face 'parenthesis)
  (ov-set "\\.eslintrc\\.yml$" 'face 'parenthesis)
  (ov-set "\\.clang-format$" 'face 'parenthesis)
  (ov-set "\\.eslintignore$" 'face 'parenthesis)
  (ov-set "\\.travis\\.yml$" 'face 'parenthesis)
  (ov-set "\\.rustfmt\\.toml$" 'face 'parenthesis)
  (ov-set "package-lock\\.json$" 'face 'parenthesis)
  (ov-set "yarn\\.lock$" 'face 'parenthesis)
  (ov-set "poetry\\.lock$" 'face 'parenthesis)
  (ov-set (rx (or "UN" "") "LICENSE" (* alnum)) 'face 'parenthesis)
  (ov-set "COPYING" 'face 'parenthesis)

  (ov-set "\\.el$" 'face 'modus-themes-intense-magenta)

  ;; programming
  (ov-set "\\.rs$" 'face 'modus-themes-intense-blue)
  (ov-set "\\.js$" 'face 'modus-themes-intense-blue)
  (ov-set "\\.fs$" 'face 'modus-themes-intense-blue)
  (ov-set "\\.c$" 'face 'modus-themes-intense-blue)
  (ov-set "\\.ts$" 'face 'modus-themes-intense-blue)
  (ov-set "\\.py$" 'face 'modus-themes-intense-blue)
  (ov-set "\\.hy$" 'face 'modus-themes-intense-blue)
  (ov-set "\\.lisp$" 'face 'modus-themes-intense-blue)
  (ov-set "\\.asd$" 'face 'modus-themes-intense-blue)
  (ov-set "\\.asd$" 'face '(:underline t))

  ;; documentation
  (ov-set "\\.org$" 'face 'modus-themes-bold)
  (ov-set "\\.org$" 'face 'modus-themes-intense-yellow)
  (ov-set "\\.info$" 'face 'modus-themes-intense-yellow)
  (ov-set "\\.pdf$" 'face 'modus-themes-intense-yellow)
  (ov-set "\\.rst$" 'face 'modus-themes-intense-yellow)
  (ov-set "\\.txt$" 'face 'modus-themes-intense-yellow)

  (ov-set "\\.nix$" 'face 'modus-themes-intense-green)
  (ov-set "\\.gmi$" 'face 'modus-themes-intense-yellow)
  (ov-set "\\.png$" 'face 'modus-themes-intense-red)
  (ov-set "\\.jpg$" 'face 'modus-themes-intense-red)
  (ov-set "\\.jpeg$" 'face 'modus-themes-intense-red)
  ;; (ov-set "\\.webm$" 'face 'modus-themes-intense-red)
  (ov-set "CHANGELOG\\.md$" 'face 'modus-themes-intense-yellow)
  (ov-set "README\\.md$" 'face 'modus-themes-intense-yellow)
  (ov-set "README\\.org$" 'face 'modus-themes-intense-yellow)
  (ov-set "README\\.rst$" 'face 'modus-themes-intense-yellow)
  (ov-set "README\\.txt$" 'face 'modus-themes-intense-yellow)
  (ov-set "README\\.md$" 'face 'modus-themes-bold)
  (ov-set "README\\.org$" 'face 'modus-themes-bold)
  (ov-set "README\\.rst$" 'face 'modus-themes-bold)
  (ov-set "README\\.txt$" 'face 'modus-themes-bold))

(add-hook 'dired-after-readin-hook #'nagy-dired-flake-underline)
(add-hook 'dired-subtree-after-insert-hook #'nagy-dired-flake-underline)

(use-package dired
  :preface
  (defun nagy-dired-mark-if-git ()
    (interactive)
    (when-let ((git-repo (file-exists-p (format "%s/.git" (car (dired-get-marked-files t)))))
               (ov (make-overlay (1- (point)) (point))))
      (ov-set ov 'face 'modus-themes-subtle-magenta)
      (if (file-exists-p (format "%s/flake.nix" (car (dired-get-marked-files t))))
          (ov-set ov 'display "⨏")))
    (dired-next-line 1))
  :custom
  (dired-deletion-confirmer #'y-or-n-p)
  (dired-do-revert-buffer t)
  (dired-compress-directory-default-suffix ".tar.zst")
  :bind
  ("s-j" . dired-jump)
  (:map dired-mode-map
        ("M-l" . dired-do-load)
        ("M-m" . dired-do-chmod))
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "f" #'dired-find-file
           "o" #'dired-find-file-other-window
           "F" #'embark-act
           "ł" #'nagy-dired-mark-if-git))

(use-package dired-subtree
  :demand t
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "e" #'dired-subtree-toggle))

(use-package dired-narrow
  :config)

(provide 'nagy-dired)
;;; nagy-dired.el ends here
