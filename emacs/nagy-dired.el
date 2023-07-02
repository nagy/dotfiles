;;; nagy-dired.el --nagy-dired config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") ov dired-subtree dired-quick-sort)
(require 'ov)



(use-package dired-subtree
  ;; :config
  ;; (map! :map dired-mode-map
  ;;       :n "e" #'dired-subtree-toggle)
  )

;; (use-package dired-quick-sort
;;   :init
;;   (add-hook 'dired-mode-hook 'dired-quick-sort))

(provide 'nagy-dired)
;;; nagy-dired.el ends here
