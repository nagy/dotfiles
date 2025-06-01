;;; nagy-dired.el --- Dired config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dired-collapse dired-narrow dired-subtree nagy-evil)

(require 'nagy-evil)                    ; to preload dired bindings
(eval-when-compile
  (require 'dired))
(declare-function dired-do-copy "dired-aux")
(declare-function dired-do-delete "dired")
(declare-function dired-do-flagged-delete "dired")
(declare-function dired-create-directory "dired-aux")
(declare-function dired-get-marked-files "dired")
(declare-function dired-get-file-for-visit "dired")

;; NIX-EMACS-PACKAGE: ov
(require 'ov)

(require 'general)
(eval-when-compile
  (require 'subr-x))

(defun nagy-dired-directory-substitute (directory)
  (declare (pure t) (side-effect-free t))
  ;; we declare it as pure, even though the #'expand-file-name function uses the HOME folder name,
  ;; which is not known at compile time
  (thread-last directory
    (string-replace (expand-file-name "~") "~")
    (string-replace "/nix/store" "○")
    (string-replace "/tmp/t" "⧖")
    ))

(defvar nagy-dired-font-lock-keywords
  `((,(rx (or ".nix") eol)
     (0 `(face modus-themes-intense-green)))
    (,(rx (group (or ".json" ".yaml" ".yml" ".toml" ".xml" ".csv"))
          (* ".zst" ".br")
          eol)
     (1 `(face modus-themes-intense-red)))
    ;; media, Photos
    (,(rx (group
           (or ".png" ".jpg" ".jpeg" ".webp" ".jxl" ".svg" ".avif"))
          eol)
     (1 `(face modus-themes-intense-cyan)))
    ;; media, Videos, audio
    (,(rx (group
           (or ".thumbs"))
          (or ".mp4" ".webm" ".gif" ".opus" ".ogg" ".mp3" ".wav" ".avi" ".m4a" ".flac")
          eol)
     (1 `(face modus-themes-nuanced-cyan)))
    (,(rx (group
           (or ".mp4" ".webm" ".gif" ".opus" ".ogg" ".mp3" ".wav" ".avi" ".m4a" ".flac"))
          eol)
     (1 `(face modus-themes-subtle-cyan)))
    (,(rx " " (group (or "default" "flake") ".nix") eol)
     (1 (progn (ov-set (make-overlay (match-beginning 1)
                                     (match-end 1))
                       'evaporate t
                       'face '(:underline t))
               nil)))
    ))

(define-minor-mode drfl-mode
  "drfl-mode."
  :lighter " drfl"
  (if drfl-mode
      (font-lock-add-keywords nil nagy-dired-font-lock-keywords 'append)
    (font-lock-remove-keywords nil nagy-dired-font-lock-keywords))
  (font-lock-flush))

(defun nagy-dired-find-file-literally ()
  (interactive)
  (set-buffer-modified-p nil)
  (find-file-literally (dired-get-file-for-visit)))

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "M-f" #'nagy-dired-find-file-literally)
  (add-hook 'dired-mode-hook #'drfl-mode))

(use-package dired
  :demand t
  :preface
  (defun +revert-when-dired (&rest _rest)
    "Revert when major-mode is dired.

Can be used as an advice."
    (when (derived-mode-p 'dired-mode)
      (call-interactively #'revert-buffer)))
  (defun nagy-dired-toggle-executable-flag ()
    (interactive)
    (dolist (file (dired-get-marked-files t))
      (cl-assert (zerop (call-process "chmod" nil nil nil
                                      (if (file-executable-p file) "-x" "+x")
                                      file))))
    (revert-buffer-quick))
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
  (dired-switches-in-mode-line 3)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh -g --no-group --group-directories-first")
  ;; (dired-listing-switches ". -l")
  :config
  (advice-add #'dired-create-directory :after #'+revert-when-dired)
  (advice-add #'dired-do-flagged-delete :after #'+revert-when-dired)
  (advice-add #'dired-do-delete :after #'+revert-when-dired)
  :bind
  ("s-j" . dired-jump)
  (:map dired-mode-map
        ("M-l" . dired-do-load)
        ("M-m" . dired-do-chmod)
        ("H-i" . dired-do-info)
        ;; ("H-m" . dired-do-man)
        ;; ("H-e" . dired-do-eww)
        ("<home>" . evil-goto-first-line)
        ("<end>" . evil-goto-line)
        )
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "f" #'dired-find-file
           "r" #'revert-buffer
           "q" #'bury-buffer
           "~" #'nagy-dired-toggle-executable-flag
           ;; "." #'terminal
           "gg" #'evil-goto-first-line
           "G" #'evil-goto-line
           "a" #'magit-status
           "H" #'evil-window-top
           "M" #'evil-window-middle
           "L" #'evil-window-bottom
           "o" #'dired-find-file-other-window
           "ö" #'browse-url-of-dired-file
           "y" #'dired-copy-filename-as-kill
           ))

(use-package dired-subtree
  ;; :after dired
  :demand t
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "e" #'dired-subtree-toggle))

(use-package dired-narrow
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "s" #'dired-narrow-regexp)
  :bind
  (:map dired-mode-map
        ("M-/" . dired-narrow-regexp))
  (:map dired-narrow-map
        ("<key-chord> f j" . exit-minibuffer)
        ("<key-chord> j f" . minibuffer-keyboard-quit)))

(defun dired-add-actual-S-switch ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " -S"))
  (revert-buffer))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "M-↓" #'dired-add-actual-S-switch))

(defun dired-add-actual-S-switch-inverted ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " -S -r"))
  (revert-buffer))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "M-↑" #'dired-add-actual-S-switch-inverted))

(defun dired-add-actual-x-switch ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " --sort=extension"))
  (revert-buffer))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "M-←" #'dired-add-actual-x-switch))

(defun dired-add-actual-x-switch-inverted ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " --sort=extension -r"))
  (revert-buffer))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "M-→" #'dired-add-actual-x-switch-inverted))

(defun dired-add-actual-t-switch ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " -t"))
  (revert-buffer))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "M-ŧ" #'dired-add-actual-t-switch-inverted))

(defun dired-add-actual-t-switch-inverted ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " -t -r"))
  (revert-buffer))

(defun dired-do-delete-force ()
  (interactive)
  (let ((delete-by-moving-to-trash
         (not (or (string-prefix-p temporary-file-directory default-directory)
                  (string-prefix-p (expand-file-name "~/.local/share/Trash/") (expand-file-name default-directory))
                  (string-prefix-p (expand-file-name "~/.cache/") (expand-file-name default-directory))
                  )))
        (dired-deletion-confirmer #'always)
        (dired-recursive-deletes 'always)
        (dired-clean-confirm-killing-deleted-buffers nil))
    (dired-do-delete)))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "H-d" #'dired-do-delete-force))

(defun nagy-dired-do-copy ()
  (interactive)
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (_prompt dir _default-filename &rest _rest)
               dir
               )))
    (dired-do-copy)))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "H-c" #'nagy-dired-do-copy))

;; (defmacro defun-dired ()
;;   "a macro to create functions, that apply to dired files.
;; marks the created function to be M-X able in dired-mode")

(provide 'nagy-dired)
;;; nagy-dired.el ends here
