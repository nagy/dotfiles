;;; nagy-kubernetes.el --- My kubernetes config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: dockerfile-mode
(use-package dockerfile-mode
  :defer t
  :bind
  ("H-M-D" . dockerfile-mode))
(defun find-file-directory-dockerfile ()
  (interactive)
  (find-file "Dockerfile")
  )
(require 'dired)
(keymap-set dired-mode-map "H-M-D" #'find-file-directory-dockerfile)

;; NIX-EMACS-PACKAGE: flymake-hadolint
(use-package flymake-hadolint
  :defer t
  )

;; NIX-EMACS-PACKAGE: terraform-mode
(use-package terraform-mode
  :preface
  (reformatter-define terraform-fmt
    :program "tofu"
    :args '("fmt" "-")
    ;; :lighter " TFmt"
    :group 'terraform-mode)
  :defer t
  :pretty 'terraform-mode
  ("data" . [?𝒅 (Br . Bl) ?𝒂])
  ("provider" . [?𝒑 (Br . Bl) ?𝒓])
  ("resource" . [?𝒓 (Br . Bl) ?𝒆])
  ("output" . [?𝒐 (Br . Bl) ?𝒑])
  :general
  (:states 'normal :keymaps 'terraform-mode-map
           "⊢" #'terraform-fmt-buffer)
  :hook
  (terraform-mode . terraform-fmt-on-save-mode)
  ;; :config
  ;; (push '(terraform-mode "terraform-ls" "serve") eglot-server-programs)
  )

;; NIX-EMACS-PACKAGE: kubed
(use-package kubed
  :defer t
  :bind
  ("H-K" . kubed-prefix-map)
  :config
  (add-to-list 'inhibit-message-regexps (rx bol "Updated Kubernetes "))
  :general
  (:states 'normal :keymaps 'kubed-list-mode-map
           [remap evil-replace]  #'kubed-list-update
           [remap revert-buffer-quick]  #'kubed-list-update
           )
  )

(provide 'nagy-kubernetes)
;;; nagy-kubernetes.el ends here
