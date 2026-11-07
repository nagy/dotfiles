;;; nagy-go.el --- Go support -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

(use-package go-ts-mode
  :defer t
  :bind
  ("H-M-g" . go-ts-mode))

(defun find-file-directory-go ()
  (interactive)
  (cond
   ((file-exists-p "main.go")
    (find-file "main.go"))
   ((file-exists-p "go.mod")
    (find-file "go.mod"))
   (t (user-error "No Go suitable file found"))))

(require 'dired)
(keymap-set dired-mode-map "H-M-g" #'find-file-directory-go)

;; NIX-EMACS-PACKAGE: go-mod-ts-extras-mode
(use-package go-mod-ts-extras-mode
  :defer t
  :hook
  (go-mod-ts-mode-hook . go-mod-ts-extras-mode))

(provide 'nagy-go)
;;; nagy-go.el ends here
