;;; nagy-dired.el --nagy-dired config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") ov general dired-narrow dired-subtree)

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
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "s" #'dired-narrow-regexp)
  :bind
  (:map dired-mode-map
        ("M-/" . dired-narrow-regexp)))
(defun dired-home ()
  (interactive)
  (dired "~"))

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


(provide 'nagy-dired)
;;; nagy-dired.el ends here
