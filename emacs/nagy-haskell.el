;;; nagy-haskell.el --- haskell config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") haskell-mode ormolu general nagy-use-package)

(require 'general)
;; (require 'nagy-use-package)

(use-package haskell-mode
  :defer t
  :defines (haskell-interactive-mode-map)
  ;; :config
  :bind
  (:map haskell-mode-map
        ("H-l" . haskell-process-load-file))
  (:map haskell-interactive-mode-map
        ([remap revert-buffer-quick] . haskell-interactive-mode-clear))
  )

(use-package ormolu
  :defer t
  ;; :config
  )

(provide 'nagy-haskell)
;;; nagy-haskell.el ends here
