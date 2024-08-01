;;; nagy-text.el --- My text config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") evil general jinx lorem-ipsum wordnut nagy-use-package)

(require 'nagy-use-package)
(require 'general)

(use-package jinx
  :general
  (:states 'normal
           "×" #'jinx-mode))

(use-package wordnut
  :defer t
  :same "^\\*WordNut\\*"
  :config
  ;; Disable header line
  (defun wordnut--headerline ()))

(use-package lorem-ipsum
  :general
  (:states 'normal
           "C-🫧" #'lorem-ipsum-insert-sentences
           "🫧" #'lorem-ipsum-insert-paragraphs))

;; TODO markdown disable toggle markup when pretty key is pressed

(provide 'nagy-text)
;;; nagy-text.el ends here
