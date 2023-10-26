;;; nagy-lsp.el --nagy-lsp config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") eglot consult-eglot general)

(require 'general)

(use-package eglot
  :demand t
  ;; :custom
  ;; (eglot-autoshutdown t)
  :config
  (setq eglot-send-changes-idle-time 0.1)
  ;; massive perf boost --- don't log every event
  (advice-add 'jsonrpc--log-event :override #'ignore)
  ;; or just (fset #'jsonrpc--log-event #'ignore)
  ;; more tips
  ;; https://old.reddit.com/r/emacs/comments/16vixg6/how_to_make_lsp_and_eglot_way_faster_like_neovim/
  (setq eglot-sync-connect nil)
  (setq eglot-events-buffer-size 0)
  :bind
  ("H-s-e" . eglot-rename)
  ("H-s-r" . eglot-inlay-hints-mode)
  ("<key-chord> ü a" . eglot-code-action-quickfix)
  :general
  (:states 'normal :prefix "æ"
           "e" #'eglot
           "a" #'eglot-code-actions
           "q" #'eglot-code-action-quickfix
           "s-k" #'eglot-shutdown-all)
  (:states 'normal :keymaps 'eglot-mode-map
           "ſ" #'eglot-code-actions
           "M-æ" #'eglot-code-actions)
  (:states 'normal :keymaps 'prog-mode-map
           "↓" #'xref-find-references
           "↑" #'xref-find-definitions))

(use-package consult-eglot)

(provide 'nagy-lsp)
;;; nagy-lsp.el ends here
