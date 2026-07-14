;;; nagy-ai.el --- AI configuration -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

(require 'dired)

;; NIX-EMACS-PACKAGE: pi-coding-agent
(use-package pi-coding-agent
  :defer t
  :preface
  (defun nagy-ai--pi-coding-agent-switch-to-input ()
    (interactive)
    (switch-to-buffer pi-coding-agent--input-buffer))
  (defun nagy-ai--pi-coding-agent-switch-to-chat ()
    (interactive)
    (switch-to-buffer pi-coding-agent--chat-buffer))
  :custom
  (pi-coding-agent-quit-without-confirmation t)
  (pi-coding-agent-thinking-display 'hidden)
  (pi-coding-agent-essential-grammar-action 'warn)
  ;; (pi-coding-agent-copy-raw-markdown t)
  ;; (pi-coding-agent-bash-preview-lines 5)
  ;; (pi-coding-agent-tool-preview-lines 10)
  :bind
  ("C-ð" . pi-coding-agent)
  ;; (:map dired-mode-map
  ;;       ("C-ð" . pi-coding-agent))
  (:map pi-coding-agent-chat-mode-map
        ;; ([remap save-kill-buffer] . pi-coding-agent-send)
        ([remap kill-this-buffer] . pi-coding-agent-quit)
        ([remap nagy-kill-this-buffer] . pi-coding-agent-quit)
        ([remap evil-append] . magit-status)
        ("s-a" . nagy-ai--pi-coding-agent-switch-to-input)
        ("H-a" . nagy-ai--pi-coding-agent-switch-to-input))
  (:map pi-coding-agent-input-mode-map
        ([remap save-kill-buffer] . pi-coding-agent-send)
        ([remap save-buffer] . pi-coding-agent-send)
        ([remap kill-this-buffer] . pi-coding-agent-quit)
        ([remap nagy-kill-this-buffer] . pi-coding-agent-quit)
        ("s-a" . nagy-ai--pi-coding-agent-switch-to-chat)
        ("H-a" . nagy-ai--pi-coding-agent-switch-to-chat)))


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
