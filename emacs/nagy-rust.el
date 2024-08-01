;;; nagy-rust.el --- rust config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") rustic reformatter general nagy-use-package)

(require 'general)

(require 'nagy-use-package)

(require 'reformatter)

(use-package rustic
  :commands (rustic-setup-lsp)
  :preface
  (reformatter-define rustfmt
    :group 'rustic
    :program "rustfmt")
  :hook
  (rustic-mode . rustfmt-on-save-mode)
  :mode
  ("Cargo\\.lock\\'" . conf-toml-mode)
  :bind
  ("H-M-r" . rustic-mode)
  (:map rustic-mode-map
        ("C-⊢" . rustfmt-buffer))
  :general
  (:states 'normal :keymaps 'rustic-mode-map
           "⊢" #'rustfmt-buffer)
  :config
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
  ("mut" . "⌿")
  ("format!" . print)
  ("return" . return)
  ("assert_eq!" . assert)
  ("std" . stdlib)
  ("use" . import)
  ("enum" . "ⅇ")
  ("const" . const)
  ("pub" . "🌐")
  ("struct" . "𝕤")
  ("impl" . "𝕚")
  ("str" . "𝕤")
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
