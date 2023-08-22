;;; nagy-lsp.el --nagy-lsp config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") eglot consult-eglot general)

(require 'general)

(use-package eglot
  :demand t
  ;; :custom
  ;; (eglot-autoshutdown t)
  :bind
  ("H-s-e" . eglot-rename)
  ("H-s-r" . eglot-inlay-hints-mode)
  :general
  (:states 'normal :prefix "æ"
           "e" #'eglot
           "a" #'eglot-code-actions
           "q" #'eglot-code-action-quickfix
           "s-k" #'eglot-shutdown-all)
  (:states 'normal :keymaps 'prog-mode-map
           "M-æ" #'eglot-code-actions
           "↓" #'xref-find-references
           "↑" #'xref-find-definitions))

(use-package consult-eglot)

(provide 'nagy-lsp)
;;; nagy-lsp.el ends here
