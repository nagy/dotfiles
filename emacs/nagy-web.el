;;; nagy-web.el --nagy-web config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") coffee-mode typescript-mode wat-mode csv-mode yaml-mode jq-mode svelte-mode nagy-use-package)

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

(use-package js
  :pretty 'js-mode
  ("true" . true) ("false" . false))

(use-package wat-mode
  :pretty 'wat-mode
  ("export" . export)
  ("module" . "📦")
  ("global" . "🌐"))

(use-package csv-mode)
(use-package yaml-mode)

(use-package jq-mode
  ;; :mode "\\.jq\\'"
  :pretty 'jq-mode
  ("def" . def)
  ("try" . try) ("catch" . except)
  :abbrev 'jq-mode
  ("d" . "def")
  ("t" . "try")
  ("c" . "catch")
  ("sel" . "select")
  ("con" . "contains"))

(use-package svelte-mode)

(provide 'nagy-web)
;;; nagy-web.el ends here
