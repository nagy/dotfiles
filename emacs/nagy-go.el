;;; nagy-go.el --- Go support -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

;; NIX-EMACS-PACKAGE: go-mode
(use-package go-mode
  :preface
  (reformatter-define go-fmt
    :group 'go
    :program "gofmt"
    :lighter " GF")
  :hook
  (go-mode . go-fmt-on-save-mode)
  :bind
  ("H-M-g" . go-mode))
;; (setq auto-insert-alist nil)
;; (define-auto-insert
;;   `("\\.go\\'" . "Go skeleton")
;;   '("Short description: "
;;     "package main;" \n
;;     \n
;;     "import \"fmt\"" \n
;;     \n
;;     "func main() {" \n
;;     "fmt.Println(\"hello world\")" \n
;;     > _ \n
;;     "}" > \n))
;; (defun find-file-directory-go ()
;;   (interactive)
;;   (find-file "main.go")
;;   )
;; (keymap-set dired-mode-map "H-M-g" #'find-file-directory-go)

(provide 'nagy-go)
;;; nagy-go.el ends here
