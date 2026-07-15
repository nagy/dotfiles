;;; nagy-data-formats.el --- Data format modes -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

(require 'general)

;; * JSON

;; ;; NIX-EMACS-PACKAGE: json-ts-mode
;; (use-package json-ts-mode
;;   :defer t
;;   )

;; (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

;; (defun my-json-ts-extra-highlights ()
;;   "Fügt explizite Fehler-Hervorhebung für Tree-sitter JSON hinzu."
;;   (setq-local treesit-font-lock-settings
;;               (append treesit-font-lock-settings
;;                       (treesit-font-lock-rules
;;                        :language 'json
;;                        :feature 'error
;;                        :override t
;;                        '((ERROR) @flymake-error)))))
;; (add-hook 'json-ts-mode-hook #'my-json-ts-extra-highlights)

;; NIX-EMACS-PACKAGE: jq-mode
(use-package jq-mode
  :preface
  (reformatter-define jqfmt
    :group 'emacs
    :program "jqfmt"
    :args '("-ob" "-ar" "-op" "pipe"))
  :defer t
  ;; :mode "\\.jq\\'"
  ;; :interpreter "jq"
  ;; :hook
  ;; (jq-mode-hook . jqfmt-on-save-mode)
  :bind
  (:map jq-mode-map
        ("C-⊢" . jqfmt-buffer))
  :general
  (:states 'normal :keymaps 'jq-mode-map
           "⊢" #'jqfmt-buffer))

;; * YAML

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


;; * TOML

(use-package toml-ts-mode
  :defer t)
  ;; :config
  ;; (add-to-list 'paren-face-modes 'toml-ts-mode)
  ;; (defvar my/toml-ts-bracket-rules
  ;;   (treesit-font-lock-rules
  ;;    :language 'toml
  ;;    :feature 'bracket
  ;;    :override t
  ;;    '((["[" "]" "{" "}"]) @nagy-subtle-yellow)))
  ;; (add-hook 'toml-ts-mode-hook
  ;;           (lambda ()
  ;;             (setq-local treesit-font-lock-settings
  ;;                         (append treesit-font-lock-settings my/toml-ts-bracket-rules))))


;; * CSV

;; NIX-EMACS-PACKAGE: csv-mode
(use-package csv-mode
  :defer t)
  ;; :custom
  ;; (csv-align-style 'centre)
  ;; :config
  ;; (csv-align-mode)


(provide 'nagy-data-formats)
;;; nagy-data-formats.el ends here
