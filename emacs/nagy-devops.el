;;; nagy-devops.el --- DevOps mode configs -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package)

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

(provide 'nagy-devops)
;;; nagy-devops.el ends here
