;;; nagy-go.el --- Go support -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

;; ;; NIX-EMACS-PACKAGE: go-mode
;; (use-package go-mode
;;   :preface
;;   (reformatter-define go-fmt
;;     :group 'go
;;     :program "gofmt"
;;     :lighter " GF")
;;   :hook
;;   (go-mode . go-fmt-on-save-mode)
;;   :bind
;;   ("H-M-g" . go-mode))

(use-package go-ts-mode
  :defer t)


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

(provide 'nagy-go)
;;; nagy-go.el ends here
