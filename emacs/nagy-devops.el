;;; nagy-devops.el --- DevOps mode configs -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general reformatter nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: openstack
(use-package openstack
  :defer t
  :init
  (nagy-same-window "^\\*OpenStack "))

;; NIX-EMACS-PACKAGE: network-modes
(use-package network-modes
  :defer t
  :init
  (nagy-same-window "^\\*net-"))

;; NIX-EMACS-PACKAGE: gitlab-ci-mode
(use-package gitlab-ci-mode
  :defer t
  :bind
  ("H-M-Y" . gitlab-ci-mode))

;; NIX-EMACS-PACKAGE: gitlab-modes
(use-package gitlab-modes
  :defer t
  :custom
  (gitlab-job-log-show-timestamps nil)
  :init
  (nagy-same-window "^\\*GitLab ")
  :general
  (:states 'normal :keymaps 'gitlab-detail-mode-map
           "a" #'magit-status)
  (:states 'normal :keymaps 'gitlab-list-mode-map
           "a" #'magit-status)
  (:states 'normal :keymaps 'gitlab-job-log-mode-map
           "a" #'magit-status))

;; NIX-EMACS-PACKAGE: dockerfile-mode
(use-package dockerfile-mode
  :defer t
  :bind
  ("H-M-D" . dockerfile-mode))
(defun find-file-directory-dockerfile ()
  (interactive)
  (find-file "Dockerfile"))

(require 'dired)
(keymap-set dired-mode-map "H-M-D" #'find-file-directory-dockerfile)

;; NIX-EMACS-PACKAGE: flymake-hadolint
(use-package flymake-hadolint
  :defer t)


;; NIX-EMACS-PACKAGE: terraform-mode
(use-package terraform-mode
  :preface
  (reformatter-define terraform-fmt
    :program "tofu"
    :args '("fmt" "-")
    ;; :lighter " TFmt"
    :group 'terraform-mode)
  :defer t
  :general
  (:states 'normal :keymaps 'terraform-mode-map
           "⊢" #'terraform-fmt-buffer)
  :hook
  (terraform-mode-hook . terraform-fmt-on-save-mode))
  ;; :config
  ;; (push '(terraform-mode "terraform-ls" "serve") eglot-server-programs)


;; NIX-EMACS-PACKAGE: kubed
(use-package kubed
  :defer t
  :bind
  ("H-K" . kubed-prefix-map)
  :config
  (add-to-list 'inhibit-message-regexps (rx bol "Updated Kubernetes "))
  :general
  (:states 'normal :keymaps 'kubed-list-mode-map
           [remap evil-ret]  #'kubed-list-select-resource
           [remap next-window-any-frame]  #'kubed-list-select-resource-other-window
           [remap evil-replace]  #'kubed-list-update
           [remap revert-buffer-quick]  #'kubed-list-update
           [remap evil-delete]  #'kubed-list-mark-for-deletion
           [remap evil-undo]  #'kubed-list-unmark
           "H-l" #'kubed-list-logs))

(provide 'nagy-devops)
;;; nagy-devops.el ends here
