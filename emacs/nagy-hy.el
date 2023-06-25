;;; nagy-hy.el --- My hy config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") hy-mode nagy-use-package)

(require 'nagy-use-package)

(use-package hy-mode
  :pretty 'hy-mode
  ("setv" . "🫧")
  ("True" . true) ("False" . false)
  ("raise" . throw)
  ("defmain" . "𝔐")
  )

(provide 'nagy-hy)
;;; nagy-hy.el ends here
