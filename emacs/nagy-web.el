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

(use-package eww
  :preface
  (defun nagy-web-eww-revert-buffer ()
    (interactive)
    (setq-local revert-buffer-function #'nagy-web-eww-revert-buffer2))

  (defun nagy-web-eww-revert-buffer2 (&rest _args)
    (interactive)
    (eww-reload))
  :bind
  ("s-€" . eww)
  ("H-j" . scroll-up-command)
  ("H-k" . scroll-down-command)
  ("H-y" . eww-copy-page-url)
  ;; :hook
  ;; (eww-mode-hook . variable-pitch-mode)
  :general
  (:states 'normal :keymaps 'eww-mode-map
           "r" #'eww-reload
           "R" #'eww-readable
           "." #'eww-browse-with-external-browser)
  (:states 'motion :keymaps 'eww-mode-map
           [remap evil-jump-backward] #'eww-back-url
           [remap evil-jump-forward] #'eww-forward-url)
  :config
  (add-hook 'eww-mode-hook #'nagy-web-eww-revert-buffer))

(use-package shr
  :defer t
  :custom
  (shr-inhibit-images t)
  (shr-image-animate nil)
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-hr-line ?⎯)
  :config
  (set-face-attribute 'shr-h1 nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)
  (set-face-attribute 'shr-h2 nil :font "Et Bembo" :height 1.5 :inherit 'modus-themes-heading-2)
  (set-face-attribute 'shr-h3 nil :font "Et Bembo" :height 1.2 :inherit 'modus-themes-heading-3))

;; NIX-EMACS-PACKAGE: osm
(use-package osm
  :custom
  (osm-copyright nil)
  (osm-max-age nil)
  :config
  (with-eval-after-load 'consult
    (defvar consult-bookmark-narrow)
    (push '(?o "OSM" osm-bookmark-jump) consult-bookmark-narrow))
  :bind
  (:map osm-mode-map
        ("<home>" . osm-left-left)
        ("<end>" . osm-right-right)
        ("<next>" . osm-down-down)
        ("<prior>" . osm-up-up))
  ;; :config
  ;; (require 'osm-ol)
  ;; (put 'osm-bookmark-jump 'bookmark-handler-type "Osm")
  ;; (evil-set-initial-state 'osm-mode 'emacs)
  :init
  (nagy-same-window "^\\*osm"))

;; NIX-EMACS-PACKAGE: ipfs-modes
(use-package cid-mode
  :defer t
  :preface
  (defun nagy-cid-mode-set-url ()
    (when cid--info
      (setq-local url-knowledge-url (concat "https://cid.ipfs.tech/#" (cid-info-cid-string cid--info)))))
  (defun nagy-cid-mode-cd ()
    (let ((path (concat "/tmp/ipfs/" (cid-info-cid-string cid--info))))
      (when (file-directory-p path)
        (cd path))))
  :init
  (nagy-same-window "^\\*CID: ")
  :hook
  (cid-decode-hook . nagy-cid-mode-set-url)
  (cid-decode-hook . nagy-cid-mode-cd)
  :bind
  (:map cid-mode-map
        ([emap eww-copy-page-url] . cid-copy-cid)))

(use-package embark-cid
  :defer t)

(provide 'nagy-web)
;;; nagy-web.el ends here
