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
  ("new" . new)
  ("let" . let)
  ("format!" . print)
  ("return" . return)
  ("assert_eq!" . assert)
  ("std" . stdlib)
  ("use" . import)
  ("Vec" . "𝕍")
  ("String" . "𝕊")
  ("Result" . "ℝ")
  ("Option" . "𝕆")
  ("unwrap" . "𝕌")
  :abbrev 'rustic-mode
  ("uw" . "unwrap")
  ("rs" . "Result")
  ("st" . "String")
  ("v" . "Vec")
  ("l" . "let")
  ("m" . "mut")
  :cycle 'rustic-mode
  ("Result" "Option"))

(provide 'nagy-rust)
;;; nagy-rust.el ends here
