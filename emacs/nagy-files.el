;;; nagy-files.el --- Files, bookmarks and revert config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-use-package)

(use-package files
  ;; :custom
  ;; (revert-without-query '("\\.json"))
  :bind
  ("s-f" . find-file))

(use-package bookmark
  :defer t
  :custom
  (bookmark-default-file (expand-file-name "~/.dotfiles/emacs-bookmarks")))

;; no general here
;; :general
;; (:states 'normal :keymaps 'bookmark-bmenu-mode-map
;;          "f" #'bookmark-bmenu-this-window)
;; These are preventing folder based bookmarks
;; (bookmark-automatically-show-annotations nil)
;; (bookmark-fringe-mark nil)

(use-package recentf
  :commands (recentf-keep-default-predicate)
  :preface
  (defun nagy-files-recentf--predicate (file)
    (pcase file
      ;; ((prefix "/nix/store") nil)
      (_ (recentf-keep-default-predicate file))))
  :defer t
  :custom
  (recentf-max-saved-items nil)
  (recentf-keep '(nagy-files-recentf--predicate)))

(use-package autorevert
  :diminish auto-revert-mode
  :custom
  (auto-revert-verbose nil)
  :hook
  (dired-mode-hook . auto-revert-mode)
  :bind
  ("C-⧖" . auto-revert-mode))

(provide 'nagy-files)
;;; nagy-files.el ends here
