;;; nagy-lua.el --- Lua and Fennel config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-use-package)

;; NIX-EMACS-PACKAGE: lua-mode
(use-package lua-mode
  :defer t
  :bind
  ("H-M-u" . lua-mode))

;; NIX-EMACS-PACKAGE: fennel-mode
(use-package fennel-mode
  :defer t
  :bind
  ("H-M-U" . fennel-mode))

(provide 'nagy-lua)
;;; nagy-lua.el ends here
