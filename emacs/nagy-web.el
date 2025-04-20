;;; nagy-web.el --- Web config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") csv-mode reformatter general nagy-use-package)

;; NIX-EMACS-PACKAGE: reformatter
(require 'reformatter)

(require 'general)

;; NIX-EMACS-PACKAGE: typescript-mode
(use-package typescript-mode
  :preface
  (reformatter-define deno-fmt
    :group 'emacs
    :program "deno"
    :args `("fmt" ,input-file))
  :defer t
  :pretty 'typescript-mode
  ("this" . self)
  ("import" . import)
  ("return" . return)
  ("new" . new)
  ("function" . def)
  ("const" . const)
  ("export" . export)
  ("string" . tostring)
  ("try" . try) ("catch" . except)
  )

(use-package js
  :preface
  (reformatter-define jq-format
    :group 'js
    :program "jq"
    :args '("--sort-keys")
    )
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

;; NIX-EMACS-PACKAGE: wat-mode
(use-package wat-mode
  ;; :preface
  ;; (reformatter-define wat-format
  ;;   :program "deno"
  ;;   :args `("run" "--allow-read" "npm:@webassemblyjs/wast-refmt" ,input-file))
  :defer t
  ;; until https://github.com/devonsparks/wat-mode/pull/3 is merged and the package is in melpa
  :commands (wat-mode)
  ;; until the package is in melpa
  :mode ("\\.wat\\'" . wat-mode)
  :config
  ;; This makes "wasm2wat" in `shell-command' buffers work.
  (push '("(module" . wat-mode) magic-fallback-mode-alist)
  :pretty 'wat-mode
  ("export" . export)
  ("func" . def)
  ("type" . "ƭ")
  ("module" . "📦")
  ("global" . "🌐")
  ("memory" . "󰘨")
  ("table" . "󰣟")
  ("import" . import)
  :bind
  ("H-M-w" . wat-mode))

;; NIX-EMACS-PACKAGE: yaml-mode
(use-package yaml-mode
  :preface
  (reformatter-define yq-format
    :group 'js
    :program "yq"
    :args `("--prettyPrint" ,(or (buffer-file-name) input-file)))
  ;; :hook
  ;; (yaml-mode . yq-format-on-save-mode)
  :bind
  ("H-M-y" . yaml-mode)
  (:map yaml-mode-map
        ("C-⊢" . yq-format-buffer))
  :pretty 'yaml-mode
  ("true" . true) ("false" . false)
  :general
  (:states 'normal :keymaps 'yaml-mode-map
           "⊢" #'yq-format-buffer))

;; NIX-EMACS-PACKAGE: jq-mode
(use-package jq-mode
  :defer t
  ;; :mode "\\.jq\\'"
  ;; :interpreter "jq"
  :pretty 'jq-mode
  ("def" . def)
  ("try" . try) ("catch" . except)
  :abbrev 'jq-mode
  ("d" . "def")
  ("t" . "try")
  ("c" . "catch")
  ("sel" . "select")
  ("con" . "contains"))

;; NIX-EMACS-PACKAGE: coffee-mode
(use-package coffee-mode
  :defer t
  :pretty 'coffee-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else))

(provide 'nagy-web)
;;; nagy-web.el ends here
