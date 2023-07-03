;;; nagy-lsp.el --nagy-lsp config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") eglot consult-eglot)

(use-package eglot
  :demand t
  :custom
  (eglot-autoshutdown t)
  :bind
  ("H-s-e" . eglot-rename)
  ("H-s-r" . eglot-inlay-hints-mode))

(use-package consult-eglot
  :config)


(provide 'nagy-lsp)
;;; nagy-lsp.el ends here
