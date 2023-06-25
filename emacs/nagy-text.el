;;; nagy-text.el --- My text config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") evil general jinx)

(require 'general)

(use-package jinx
  :general
  (:states 'normal
           "×" #'jinx-mode))

(provide 'nagy-text)
;;; nagy-text.el ends here
