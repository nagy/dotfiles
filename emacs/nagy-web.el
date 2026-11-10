;;; nagy-web.el --- Web development -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general reformatter nagy-use-package)

(require 'general)

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
    :program "jq")
  :bind
  ("H-M-j" . js-json-mode)
  (:map js-json-mode-map
        ("C-⊢" . jq-format-buffer))
  :hook
  (js-json-mode-hook . jq-format-on-save-mode)
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

;; NIX-EMACS-PACKAGE: link-hint
(use-package link-hint
  :bind
  ("H-a" . link-hint-open-link))

(use-package browse-url
  :commands (nagy-browse-url-at-point)
  :config
  (defun nagy-browse-url-at-point ()
    "No properties in string"
    (thing-at-point 'url t))
  ;; To only find actual URLs and not treat any word as a hostname
  (advice-add 'browse-url-url-at-point :override #'nagy-browse-url-at-point))

;; NIX-EMACS-PACKAGE: request
(use-package request
  :custom
  ;; (request-backend 'curl)
  (request-backend 'url-retrieve))

;; NIX-EMACS-PACKAGE: elpher
(use-package elpher
  :preface
  (declare-function bookmark-prop-get "bookmark")
  (defun elpher-bookmark-handler (record)
    (elpher-go (bookmark-prop-get record 'location)))
  (defun elpher-bookmark-make-record ()
    (cons "elpher"
          `((location . ,(url-recreate-url (cadr elpher-current-page)))
            (handler . elpher-bookmark-handler))))
  :commands (elpher-go)
  :functions (elpher-redraw)
  :custom
  (elpher-use-header nil)
  (elpher-ipv4-always t)
  (elpher-connection-timeout 10)
  :config
  (put 'elpher-bookmark-handler 'bookmark-handler-type "Elpher")
  (add-hook 'elpher-mode-hook
            (defun +nagy/elpher-hook ()
              (setq-local bookmark-make-record-function #'elpher-bookmark-make-record)))
  ;; (setq-local revert-buffer-function (cmd! (elpher-redraw)))

  (set-face-attribute 'elpher-gemini-heading1 nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)
  (set-face-attribute 'elpher-gemini-heading2 nil :font "Et Bembo" :height 1.5 :inherit 'modus-themes-heading-2)
  (set-face-attribute 'elpher-gemini-heading3 nil :font "Et Bembo" :height 1.2 :inherit 'modus-themes-heading-3)
  (set-face-attribute 'elpher-gemini-preformatted nil :extend t :inherit 'nagy-nuanced-green)
  :general
  (:states 'normal :keymaps 'elpher-mode-map
           "q" #'quit-window
           "M-n" #'elpher-next-link
           "M-p" #'elpher-prev-link)
  (:states 'motion :keymaps 'elpher-mode-map
           ;; (define-key elpher-mode-map "f" nil) ; this might be needed
           "f" #'push-button
           "s" #'elpher-back
           "C-o" #'elpher-back))

(provide 'nagy-web)
;;; nagy-web.el ends here
