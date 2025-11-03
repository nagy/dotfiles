;;; nagy-rust.el --- rust config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: rustic
(use-package rustic
  :commands (rustic-setup-lsp)
  :preface
  (reformatter-define rustfmt
    :group 'rustic
    :program "rustfmt"
    :args `("--edition" "2024"))
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

;; (define-auto-insert
;;   `(,(rx ".rs" eos) . "Rust skeleton")
;;    '("Short description: "
;;      "use std::io;" \n
;;      \n
;;      "pub fn main() {" \n
;;      "println!(\"Hello, World!\\n\");" \n
;;      > _ \n
;;      "}" > \n))

(provide 'nagy-rust)
;;; nagy-rust.el ends here
