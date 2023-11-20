;;; nagy-dired.el --nagy-dired config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") ov general dired-narrow dired-subtree)

(require 'dired)
(require 'ov)
(require 'general)

(defun nagy-dired-flake-underline ()
  (interactive)
  (ov-set (rx "flake.nix") 'face '(:underline t))
  (ov-set (rx "default.nix") 'face '(:underline t))
  ;; uninteresting, may also be moved to dired-omit mode
  (ov-set (rx "flake.lock" eol) 'face 'parenthesis)
  (ov-set (rx "Cargo.lock" eol) 'face 'parenthesis)
  (ov-set (rx "Gemfile.lock" eol) 'face 'parenthesis)
  (ov-set (rx ".dir-locals.el" eol) 'face 'parenthesis)
  (ov-set (rx ".gitignore" eol) 'face 'parenthesis)
  (ov-set (rx ".dockerignore" eol) 'face 'parenthesis)
  (ov-set (rx ".eslintrc.yml" eol) 'face 'parenthesis)
  (ov-set (rx ".clang-format" eol) 'face 'parenthesis)
  (ov-set (rx ".eslintignore" eol) 'face 'parenthesis)
  (ov-set (rx ".travis.yml" eol) 'face 'parenthesis)
  (ov-set (rx ".rustfmt.toml" eol) 'face 'parenthesis)
  (ov-set (rx ".cache" eol) 'face 'parenthesis)
  (ov-set (rx "package-lock.json" eol) 'face 'parenthesis)
  (ov-set (rx "yarn.lock" eol) 'face 'parenthesis)
  (ov-set (rx "poetry.lock" eol) 'face 'parenthesis)
  (ov-set (rx (or "UN" "") "LICENSE" (* alnum)) 'face 'parenthesis)
  (ov-set (rx "COPYING") 'face 'parenthesis)

  (ov-set (rx ".el" eol) 'face 'modus-themes-intense-magenta)

  ;; code
  (ov-set (rx ".rs" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".js" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".fs" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".c" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".cc" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".groovy" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".zig" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".go" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".ts" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".py" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".jq" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".hy" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".lisp" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".asd" eol) 'face 'modus-themes-intense-blue)
  (ov-set (rx ".asd" eol) 'face '(:underline t))
  (ov-set (rx ".nix" eol) 'face 'modus-themes-intense-green)

  ;; documentation
  (ov-set (rx ".machine.org" eol) 'face 'modus-themes-subtle-yellow)
  (ov-set (rx ".org" eol) 'face '(modus-themes-intense-yellow bold))
  (ov-set (rx ".md" eol) 'face 'modus-themes-intense-yellow)
  (ov-set (rx ".info" eol) 'face 'modus-themes-intense-yellow)
  (ov-set (rx ".pdf") 'face 'modus-themes-intense-yellow)
  (ov-set (rx ".rst" eol) 'face 'modus-themes-intense-yellow)
  (ov-set (rx ".rsync.txt") 'face 'modus-themes-subtle-yellow)
  (ov-set (rx ".jenkins.txt") 'face 'modus-themes-subtle-yellow)
  (ov-set (rx ".txt") 'face 'modus-themes-intense-yellow)
  (ov-set (rx "README."
              (or "md" "org" "rst" "txt") eol)
          'face '(bold modus-themes-intense-yellow))
  (ov-set (rx ".gmi" eol) 'face 'modus-themes-intense-yellow)
  (ov-set (rx "CHANGELOG.md" eol) 'face 'modus-themes-intense-yellow)

  ;; data
  (ov-set (rx ".tokei.json") 'face 'modus-themes-subtle-red)
  (ov-set (rx ".restic.json") 'face 'modus-themes-subtle-red)
  (ov-set (rx ".rclone.json") 'face 'modus-themes-subtle-red)
  (ov-set (rx ".ncdu.json") 'face 'modus-themes-subtle-red)
  (ov-set (rx ".info.json") 'face 'modus-themes-subtle-red)
  (ov-set (rx ".journal.json") 'face 'modus-themes-subtle-red)
  (ov-set (rx ".jc.json") 'face 'modus-themes-subtle-red)
  (ov-set (rx ".json") 'face 'modus-themes-intense-red)
  (ov-set (rx (or ".yml" ".yaml")) 'face 'modus-themes-intense-red)
  (ov-set (rx ".xml") 'face 'modus-themes-intense-red)
  (ov-set (rx (or ".tar" ".zip" ".nar")) 'face 'eshell-ls-archive)
  (ov-set (rx (or ".zst" ".gz" ".bz2" ".xz" ".gitbundle")) 'face '(:background "grey" :foreground "black"))

  ;; media
  (ov-set (rx ".png" eol) 'face 'modus-themes-intense-cyan)
  (ov-set (rx ".jpg" eol) 'face 'modus-themes-intense-cyan)
  (ov-set (rx ".jpeg" eol) 'face 'modus-themes-intense-cyan)
  (ov-set (rx ".jxl" eol) 'face 'modus-themes-intense-cyan)
  (ov-set (rx ".webp" eol) 'face 'modus-themes-intense-cyan)
  (ov-set (rx ".mp4" eol) 'face 'modus-themes-subtle-cyan)
  (ov-set (rx ".opus" eol) 'face 'modus-themes-subtle-cyan)
  (ov-set (rx ".ogg" eol) 'face 'modus-themes-subtle-cyan)

  ;; wasm
  (ov-set (rx ".wasm" eol) 'face '(modus-themes-fg-blue-intense modus-themes-subtle-blue))
  (ov-set (rx ".wat" eol) 'face '(modus-themes-fg-blue-intense modus-themes-subtle-blue))
  )

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
  (dired-dwim-target t)
  (dired-recursive-copies  'always)
  (dired-create-destination-dirs 'ask)
  (dired-deletion-confirmer #'y-or-n-p)
  (dired-keep-marker-rename nil)
  (dired-keep-marker-copy nil)
  (dired-do-revert-buffer t)
  (dired-free-space nil)
  (dired-compress-directory-default-suffix ".tar.zst")
  :bind
  ("s-j" . dired-jump)
  (:map dired-mode-map
        ("M-l" . dired-do-load)
        ("M-m" . dired-do-chmod)
        ("H-i" . dired-do-info)
        ("H-m" . dired-do-man)
        ("H-e" . dired-do-eww))
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
  :bind
  (:map dired-mode-map
        ("M-/" . dired-narrow-regexp)))

(provide 'nagy-dired)
;;; nagy-dired.el ends here
