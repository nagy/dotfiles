;;; nagy-window.el --- Window and frame config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-use-package)

(use-package tab-bar
  :config
  (tab-bar-mode)
  :custom
  (tab-bar-show 1)
  (tab-bar-auto-width t)
  ;; (tab-bar-auto-width-max '(330 30))
  ;; (tab-bar-auto-width-max '(440 40))
  ;; using this instead of 10000 below means no flickering in the tab bar
  (tab-bar-auto-width-max `(,(* 8 880) ,(* 8 80)))
  ;; (tab-bar-auto-width-max '(10000 1000))
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice t))

(use-package winner
  :custom
  ;; without this, in eat the C-C keys are incorrectly bound.
  (winner-dont-bind-my-keys t)          ; but does not seem to work
  :bind
  ("s-u" . winner-undo)
  ("s-U" . winner-redo))

(use-package repeat
  :bind
  ("M-s-." . repeat))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width 2)
  :bind
  ("M-ĸ" . display-line-numbers-mode)
  ("s-M-ĸ" . global-display-line-numbers-mode))

(use-package hl-line
  :bind
  ("M-ħ" . hl-line-mode))

(provide 'nagy-window)
;;; nagy-window.el ends here
