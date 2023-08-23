;;; nagy-rust.el --nagy-rust config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") rustic nagy-use-package)

(require 'nagy-use-package)
(require 'rustic)

(use-package rustic
  :mode
  ("Cargo\\.lock\\'" . conf-toml-mode)
  :init
  (require 'rustic-lsp)
  ;; (setq rustic-lsp-setup-p nil)
  (remove-hook 'rustic-mode-hook #'rustic-setup-lsp)
  ;; :config
  ;; (setq rustic-lsp-client 'eglot)
  :pretty 'rustic-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else)
  ("self" . self)
  ("fn" . def)
  ("let" . let)
  ("format!" . print)
  ("assert_eq!" . assert)
  ("std" . stdlib)
  ("use" . import))

(provide 'nagy-rust)
;;; nagy-rust.el ends here
