;;; nagy-lsp.el --- Language server config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") evil eglot consult-eglot general)

(require 'general)

(use-package eglot
  :defer t
  :custom
  ;; more tips
  ;; https://old.reddit.com/r/emacs/comments/16vixg6/how_to_make_lsp_and_eglot_way_faster_like_neovim/
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-send-changes-idle-time 0.1)
  (eglot-autoshutdown t)
  :config
  ;; massive perf boost --- don't log every event
  (advice-add 'jsonrpc--log-event :override #'ignore)
  ;; or just (fset #'jsonrpc--log-event #'ignore)
  :bind
  ("H-s-e" . eglot-rename)
  ("H-s-r" . eglot-inlay-hints-mode)
  ("<key-chord> ü a" . eglot-code-action-quickfix)
  :general
  (:states 'normal :prefix "æ"
           "e" #'eglot
           "a" #'eglot-code-actions
           "q" #'eglot-code-action-quickfix
           "æ" #'eglot-code-action-quickfix
           "s-k" #'eglot-shutdown-all
           "⊢" #'eglot-format-buffer)
  (:states 'normal :keymaps 'eglot-mode-map
           "ſ" #'eglot-code-actions
           "M-æ" #'eglot-code-actions))

;; (use-package consult-eglot)

(use-package flymake
  :defer t
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short))

(provide 'nagy-lsp)
;;; nagy-lsp.el ends here
