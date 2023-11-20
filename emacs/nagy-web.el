;;; nagy-web.el --nagy-web config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") coffee-mode typescript-mode wat-mode csv-mode yaml-mode jq-mode svelte-mode reformatter general nagy-use-package)

(require 'reformatter)

(require 'general)
(require 'nagy-use-package)

(use-package coffee-mode
  :defer t
  :pretty 'coffee-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else))

(use-package typescript-mode
  :defer t
  :pretty 'typescript-mode
  ("this" . self)
  ("import" . import)
  ("return" . return)
  ("new" . new)
  ("function" . def)
  ("const" . const))

(use-package js
  :preface
  (reformatter-define jq-format
    :group 'js
    :program "jq"
    :args '("--sort-keys"))
  :pretty 'js-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else)
  ("function" . def)
  ("return" . return)
  ("while" . loop)
  ("var" . let)
  ("const" . const)
  :bind
  ("H-M-j" . js-json-mode)
  (:map js-json-mode-map
        ("C-⊢" . jq-format-buffer))
  :hook
  (js-json-mode . jq-format-on-save-mode)
  :general
  (:states 'normal :keymaps 'js-json-mode-map
           "⊢" #'jq-format-buffer))

(use-package wat-mode
  :pretty 'wat-mode
  ("export" . export)
  ("func" . def)
  ("type" . "ƭ")
  ("module" . "📦")
  ("global" . "🌐")
  ("memory" . "󰘨")
  ("table" . "󰣟"))

(use-package yaml-mode
  :preface
  (reformatter-define yq-format
    :group 'js
    :program "yq"
    :args '("--prettyPrint"))
  :hook
  (yaml-mode . yq-format-on-save-mode)
  :bind
  ("H-M-y" . yaml-mode)
  (:map yaml-mode-map
        ("C-⊢" . yq-format-buffer))
  :general
  (:states 'normal :keymaps 'yaml-mode-map
           "⊢" #'yq-format-buffer))

(use-package jq-mode
  ;; :mode "\\.jq\\'"
  :pretty 'jq-mode
  ("def" . def)
  ("try" . try) ("catch" . except)
  :abbrev 'jq-mode
  ("d" . "def")
  ("t" . "try")
  ("c" . "catch")
  ("sel" . "select")
  ("con" . "contains"))

(provide 'nagy-web)
;;; nagy-web.el ends here
