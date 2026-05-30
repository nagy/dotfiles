;;; nagy-web.el --- Web development -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: typescript-mode
(use-package typescript-mode
  :preface
  (reformatter-define deno-fmt
    :group 'emacs
    :program "deno"
    :args `("fmt" "-"))
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
  :cycle 'typescript-mode
  ("let" "const")
  :general
  (:states 'normal :keymaps 'typescript-mode-map
           "⊢" #'deno-fmt-buffer)
  ;; :hook
  ;; (typescript-mode . deno-fmt-on-save-mode) ;; this breaks svelte mode down because that inherits typescript-mode
  )

;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
;;                  . ("deno" "lsp" :initializationOptions (:enable t :lint t :suggest.autoImports t))))
;;   ;; optional: Deno specific options adjustment
;;   ;; (setq-default eglot-workspace-configuration
;;   ;;               '(:deno (:enable t :lint t :unstable t))))
;; )


(use-package js
  :preface
  (reformatter-define jq-format
    :group 'js
    :program "jq"
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
  (add-to-list 'magic-fallback-mode-alist '("(module" . wat-mode))
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


;; (add-to-list 'eglot-server-programs
;;              '((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
;;                . ("deno" "lsp" :initializationOptions (:enable t
;;                                                                ;; :unstable t
;;                                                                :lint t
;;                                                                :typescript
;;                                                                (:inlayHints
;;                                                                 (:variableTypes (:enabled t)
;;                                                                  :parameterTypes (:enabled t)
;;                                                                  :parameterNames (:enabled "all")
;;                                                                  :functionLikeReturnTypes (:enabled t)))))))

(provide 'nagy-web)
;;; nagy-web.el ends here
