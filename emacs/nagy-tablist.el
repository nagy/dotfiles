;;; nagy-tablist.el --- My tablist config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") tablist)

(use-package tablist
  :bind
  (:map tablist-mode-map
        ("M-r" . tablist-pop-filter)
        ("M-/" . tablist-push-regexp-filter)))

(use-package stillness-mode
  ;; :ensure t
  :config
  (stillness-mode 1)
  )

(provide 'nagy-tablist)
;;; nagy-tablist.el ends here
