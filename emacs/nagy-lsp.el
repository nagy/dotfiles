;;; nagy-lsp.el --nagy-lsp config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") eglot consult-eglot)

(require 'nagy-use-package)

(use-package eglot
  :custom
  (eglot-autoshutdown t))
(use-package consult-eglot
  :config)


(provide 'nagy-lsp)
;;; nagy-lsp.el ends here
