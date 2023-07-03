;;; nagy-web.el --nagy-web config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") coffee-mode jq-mode typescript-mode nagy-use-package)

(require 'nagy-use-package)

(use-package coffee-mode
  :defer t
  :pretty 'coffee-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else))

(use-package typescript-mode
  :defer t
  :pretty 'typescript-mode
  ("this" . self))

(provide 'nagy-web)
;;; nagy-web.el ends here
