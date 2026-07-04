;;; nagy-ai.el --- AI configuration -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

(defvar dired-mode-map)

;; NIX-EMACS-PACKAGE: pi-coding-agent
(use-package pi-coding-agent
  :defer t
  :custom
  (pi-coding-agent-quit-without-confirmation t)
  (pi-coding-agent-thinking-display 'hidden)
  ;; (pi-coding-agent-bash-preview-lines 5)
  ;; (pi-coding-agent-tool-preview-lines 10)
  :bind
  (:map dired-mode-map
        ("C-ð" . pi-coding-agent))
  (:map pi-coding-agent-chat-mode-map
        ;; ([remap save-kill-buffer] . pi-coding-agent-send)
        ([remap kill-this-buffer] . pi-coding-agent-quit)
        ([remap nagy-kill-this-buffer] . pi-coding-agent-quit))
  (:map pi-coding-agent-input-mode-map
        ([remap save-kill-buffer] . pi-coding-agent-send)
        ([remap kill-this-buffer] . pi-coding-agent-quit)
        ([remap nagy-kill-this-buffer] . pi-coding-agent-quit))
  )

;; ;; NIX-EMACS-PACKAGE: gptel
;; ;; (use-package gptel)

;; ;; NIX-EMACS-PACKAGE: mcp
;; ;; (use-package mcp)

;; ;; ;; NIX-EMACS-PACKAGE: agent-shell
;; (use-package agent-shell
;;   :defer t
;;   )

(provide 'nagy-ai)
;;; nagy-ai.el ends here
