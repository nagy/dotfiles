;;; nagy-tablist.el --- My tablist config -*- lexical-binding: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") tablist general nagy-use-package)

(require 'general)

(require 'nagy-use-package)

(use-package tablist
  :bind
  (:map tablist-mode-map
        ("M-r" . tablist-pop-filter)
        ("M-/" . tablist-push-regexp-filter)))

(provide 'nagy-tablist)
;;; nagy-tablist.el ends here
