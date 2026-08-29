;;; nagy-ai.el --- AI configuration -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") evil)

(require 'dired)
(require 'evil)

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

(declare-function comint-send-input "comint")
;; NIX-EMACS-PACKAGE: agent-shell
(use-package agent-shell
  :custom
  ;; (agent-shell-prefer-viewport-interaction t))
  (agent-shell-show-config-icons nil)
  (agent-shell-show-welcome-message nil)
  (agent-shell-header-style 'text)  ;; or nil for no header
  (agent-shell-text-file-capabilities nil)
  (agent-shell-session-strategy 'new)
  (agent-shell-agent-configs '(agent-shell-pi-make-agent-config))
  (agent-shell-preferred-agent-config 'pi)
  (agent-shell-confirm-interrupt nil)
  :defer t
  :config
  ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)
  ;; Configure *agent-shell-diff* buffers to start in Emacs state
  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                (evil-emacs-state))))
  :bind
  ("C-Ð" . agent-shell)
  (:map agent-shell-mode-map
        ("s-a" . agent-shell-other-buffer)
        ("H-a" . agent-shell-other-buffer)
        ([remap revert-buffer-quick] . agent-shell-clear-buffer))
  (:map agent-shell-viewport-edit-mode-map
        ("s-a" . agent-shell-other-buffer)
        ("H-a" . agent-shell-other-buffer)
        ([remap kill-this-buffer] . agent-shell-viewport-compose-cancel)
        ([remap nagy-kill-this-buffer] . agent-shell-viewport-compose-cancel)))

;; NIX-EMACS-PACKAGE: agent-shell-links
(use-package agent-shell-links
  :defer t)

(provide 'nagy-ai)
;;; nagy-ai.el ends here
