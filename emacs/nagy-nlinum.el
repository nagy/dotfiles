;;; nagy-nlinum.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width 2)
  :bind
  ("M-ĸ" . display-line-numbers-mode)
  ("s-M-ĸ" . global-display-line-numbers-mode))

(provide 'nagy-nlinum)
;;; nagy-nlinum.el ends here
