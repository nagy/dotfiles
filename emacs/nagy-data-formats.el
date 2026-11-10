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

(use-package yaml-ts-mode
  :defer t
  :preface
  (reformatter-define yq-format
    :group 'js
    :program "yq"
    :args `("--prettyPrint" ,(or (buffer-file-name) input-file)))
  :hook
  (yaml-ts-mode-hook . yq-format-on-save-mode)
  :bind
  ("H-M-y" . yaml-ts-mode))

(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

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


(add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))

;; NIX-EMACS-PACKAGE: toml-ts-cargo-mode
(use-package toml-ts-cargo-mode
  :defer t
  ;; :custom
  ;; (toml-ts-cargo-mode-...)
  :hook
  (toml-ts-mode-hook . toml-ts-cargo-mode))

;; * CSV

;; NIX-EMACS-PACKAGE: csv-mode
(use-package csv-mode
  :defer t
  ;; :custom
  ;; (csv-align-style 'centre)
  ;; :config
  ;; (csv-align-mode)
  :bind
  ("H-M-c" . csv-mode))


;; NIX-EMACS-PACKAGE: pandoc
(use-package pandoc
  :preface
  (defun nagy-text-to-plain ()
    (interactive)
    (pcase-exhaustive major-mode
      ('mhtml-mode
       (let ((buffer (generate-new-buffer (concat "Pandoc Plain: " (buffer-name)))))
         (call-process-region nil nil "pandoc" nil buffer nil
                              "--to=plain"
                              "--wrap=none"
                              "--from=html")
         (switch-to-buffer  buffer)
         (goto-char (point-min))
         (text-mode)))))
  ;; (olivetti-mode)
  ;; (let ((inhibit-message t))
  ;;   (olivetti-set-width 80))

  ;; (evil-global-set-key 'motion (kbd "g H-M-t") #'nagy-text-to-plain)
  (defun nagy-text-to-org ()
    (interactive)
    (pcase-exhaustive major-mode
      ('mhtml-mode
       (let ((buffer (generate-new-buffer (concat "Pandoc Org: " (buffer-name)))))
         (call-process-region nil nil "pandoc" nil buffer nil
                              "--to=org"
                              "--wrap=none"
                              "--from=html")
         (switch-to-buffer  buffer)
         (goto-char (point-min))
         (org-mode))))))

;; (evil-global-set-key 'motion (kbd "g H-M-o") #'nagy-text-to-org)

;; NIX-EMACS-PACKAGE: units-mode
(use-package units-mode
  :defer t)

(provide 'nagy-data-formats)
;;; nagy-data-formats.el ends here
