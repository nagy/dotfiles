;;; nagy-use-emacs.el --- My emacs config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") benchmark-init nagy-use-package)

(require 'nagy-use-package)

(use-package emacs
  :preface
  (defun nagy-emacs-set-lexical ()
    (interactive)
    (setq lexical-binding t))
  :demand t
  :bind
  ("H-M-e" . emacs-lisp-mode))
  ;;             :eval-when-compile "ℂ"
  ;; :config
  ;; (add-hook! emacs-lisp-mode
  ;;   ;; This adds the paragraph symbol as valid function character
  ;;   (set-char-table-range (syntax-table) ?§ '(2)))


;; (when init-file-debug
;;   (require 'benchmark-init)
;;   ;;(add-hook 'doom-first-input-hook #'benchmark-init/deactivate)
;;   )

(defun find-file-make-file ()
  (interactive)
  (find-file "Makefile"))

(require 'dired)
(keymap-set dired-mode-map "H-M-M" #'find-file-make-file)

(provide 'nagy-use-emacs)
;;; nagy-use-emacs.el ends here
