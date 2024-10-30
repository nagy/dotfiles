;;; nagy-dired.el --- Dired config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") ov general dired-collapse dired-narrow dired-subtree nagy-evil)

(require 'dired)
(require 'ov)
(require 'general)
(eval-when-compile
  (require 'subr-x))

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
  (dired-switches-in-mode-line 3)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-I systemd-private-* -I .ICE-unix -I .font-unix -I .XIM-unix -I .X11-unix -I nix-build-* -h --almost-all -l  -g --no-group --group-directories-first")
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
        ("H-m" . dired-do-man)
        ("H-e" . dired-do-eww)
        ("<home>" . evil-goto-first-line)
        ("<end>" . evil-goto-line)
        )
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "f" #'dired-find-file
           "r" #'revert-buffer
           "q" #'bury-buffer
           "gg" #'evil-goto-first-line
           "G" #'evil-goto-line
           "a" #'magit-status
           "H" #'evil-window-top
           "M" #'evil-window-middle
           "L" #'evil-window-bottom
           "o" #'dired-find-file-other-window
           "ö" #'browse-url-of-dired-file))

(use-package dired-subtree
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
(keymap-set dired-mode-map "M-↓" #'dired-add-actual-S-switch)

(defun dired-add-actual-S-switch-inverted ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " -S -r"))
  (revert-buffer))
(keymap-set dired-mode-map "M-↑" #'dired-add-actual-S-switch-inverted)

(defun dired-add-actual-x-switch ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " --sort=extension"))
  (revert-buffer))
(keymap-set dired-mode-map "M-←" #'dired-add-actual-x-switch)

(defun dired-add-actual-x-switch-inverted ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " --sort=extension -r"))
  (revert-buffer))
(keymap-set dired-mode-map "M-→" #'dired-add-actual-x-switch-inverted)

(defun dired-add-actual-t-switch ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " -t"))
  (revert-buffer))
(keymap-set dired-mode-map "M-ŧ" #'dired-add-actual-t-switch-inverted)

(defun dired-add-actual-t-switch-inverted ()
  (interactive)
  (setq-local dired-actual-switches (concat dired-listing-switches " -t -r"))
  (revert-buffer))

(defun dired-do-delete-force ()
  (interactive)
  (let ((delete-by-moving-to-trash
         (not (or (string-prefix-p temporary-file-directory default-directory)
                  (string-prefix-p (expand-file-name "~/.local/share/Trash/") default-directory))))
        (dired-deletion-confirmer #'always)
        (dired-recursive-deletes 'always)
        (dired-clean-confirm-killing-deleted-buffers nil))
    (dired-do-delete)))
(keymap-set dired-mode-map
            "H-d" #'dired-do-delete-force)

(defun nagy-dired-do-copy ()
  (interactive)
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (_prompt dir _default-filename &rest _rest)
               dir
               )))
    (dired-do-copy)))
(keymap-set dired-mode-map "H-c" #'nagy-dired-do-copy)

;; (defmacro defun-dired ()
;;   "a macro to create functions, that apply to dired files.
;; marks the created function to be M-X able in dired-mode")

(provide 'nagy-dired)
;;; nagy-dired.el ends here
