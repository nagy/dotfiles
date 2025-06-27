;;; nagy-zig.el --- Zig config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: zig-mode
(use-package zig-mode
  :custom
  (zig-format-on-save nil)
  :pretty 'zig-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else) ("then" . then)
  ("fn" . def)
  ("const" . const)
  ("while" . loop)
  ("void" . null)
  ;; ("try" . try) ("catch" . except)
  ("pub" . "🌐")
  ("var" . "𝕧")
  ("bool" . "𝒃")
  ("struct" . "𝕤")
  ("return" . return)
  ("export" . export)
  :bind
  ("H-M-z" . zig-mode)
  (:map zig-mode-map
        ("C-⊢" . zig-format-buffer))
  :cycle 'zig-mode
  ("const" "var")
  :hook
  (zig-mode . zig-format-on-save-mode)
  :general
  (:states 'normal :keymaps 'zig-mode-map
           "⊢" #'zig-format-buffer))

;; (define-auto-insert
;;   `(,(rx ".zig" eos) . "Zig skeleton")
;;    '("Short description: "
;;      "const std = @import(\"std\");" \n
;;      \n
;;      "pub fn main() !void {" \n
;;      "std.debug.print(\"Hello, World!\\n\", .{});" \n
;;      > _ \n
;;      "}" > \n))

(provide 'nagy-zig)
;;; nagy-zig.el ends here
