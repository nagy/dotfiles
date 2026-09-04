;;; nagy-ai.el --- AI configuration -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") evil)

(require 'dired)
(require 'evil)

;; New name: Pilish https://github.com/dnouri/pilish/releases/tag/v3.0.0
;; Readme section how to upgrade : https://github.com/dnouri/pilish#upgrading-from-pi-coding-agent-%EF%B8%8F
;; NIX-EMACS-PACKAGE: pilish
(use-package pilish
  :defer t
  :preface
  (defun nagy-ai--pilish-switch-to-input ()
    (interactive)
    (switch-to-buffer pilish--input-buffer))
  (defun nagy-ai--pilish-switch-to-chat ()
    (interactive)
    (switch-to-buffer pilish--chat-buffer))
  :custom
  (pilish-quit-without-confirmation t)
  (pilish-thinking-display 'hidden)
  (pilish-essential-grammar-action 'warn)
  ;; (pilish-copy-raw-markdown t)
  ;; (pilish-bash-preview-lines 5)
  ;; (pilish-tool-preview-lines 10)
  :bind
  ("C-ð" . pilish)
  ;; (:map dired-mode-map
  ;;       ("C-ð" . pilish))
  (:map pilish-chat-mode-map
        ;; ([remap save-kill-buffer] . pilish-send)
        ([remap kill-this-buffer] . pilish-quit)
        ([remap nagy-kill-this-buffer] . pilish-quit)
        ([remap evil-append] . magit-status)
        ("s-a" . nagy-ai--pilish-switch-to-input)
        ("H-a" . nagy-ai--pilish-switch-to-input))
  (:map pilish-input-mode-map
        ([remap save-kill-buffer] . pilish-send)
        ([remap save-buffer] . pilish-send)
        ([remap kill-this-buffer] . pilish-quit)
        ([remap nagy-kill-this-buffer] . pilish-quit)
        ("s-a" . nagy-ai--pilish-switch-to-chat)
        ("H-a" . nagy-ai--pilish-switch-to-chat)))

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
