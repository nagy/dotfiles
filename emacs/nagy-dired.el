;;; nagy-dired.el --nagy-dired config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") ov general dired-narrow dired-subtree dired-quick-sort)

(require 'dired)
(require 'ov)
(require 'general)

(use-package dired-subtree
  :demand t
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "e" #'dired-subtree-toggle))

;; (use-package dired-quick-sort
;;   :init
;;   (add-hook 'dired-mode-hook 'dired-quick-sort))

(use-package dired-narrow
  :config)

(provide 'nagy-dired)
;;; nagy-dired.el ends here
