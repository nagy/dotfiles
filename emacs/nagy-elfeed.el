;;; nagy-elfeed.el --- nagy-elfeed config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") elfeed general nagy-use-package)

(require 'general)

(require 'nagy-use-package)

(use-package elfeed
  :custom
  (elfeed-show-truncate-long-urls nil)
  (elfeed-search-filter "+unread")
  (elfeed-search-title-max-width 100)
  (elfeed-curl-max-connections 1)
  :config
  (put 'elfeed-search-bookmark-handler 'bookmark-handler-type "Elfeed Search")
  :bind
  (:map elfeed-search-mode-map
        ("M-↓" . elfeed-search-fetch))
  :general
  (:states 'normal :keymaps 'elfeed-search-mode-map
           "r" #'elfeed-search-untag-all-unread
           "↓" #'elfeed-search-fetch)
  (:keymaps 'elfeed-search-mode-map
            [remap kill-this-buffer] #'elfeed-db-unload
            [remap save-kill-buffer] #'elfeed-db-unload)
  ;; :same "^\\*elfeed-entry"
  )

(provide 'nagy-elfeed)
;;; nagy-elfeed.el ends here
