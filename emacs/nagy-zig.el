;;; nagy-zig.el --nagy-zig config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") zig-mode general nagy-use-package)

(require 'general)

(require 'nagy-use-package)

;; (require 'reformatter)

(use-package zig-mode
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
  :general
  (:states 'normal :keymaps 'zig-mode-map
           "⊢" #'zig-format-buffer))

(provide 'nagy-zig)
;;; nagy-zig.el ends here
