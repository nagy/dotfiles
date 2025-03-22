;;; nagy-zig.el --- Zig config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") zig-mode general nagy-use-package)

(require 'general)
;; (require 'nagy-use-package)

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

(provide 'nagy-zig)
;;; nagy-zig.el ends here
