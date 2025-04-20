;;; nagy-haskell.el --- haskell config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: haskell-mode
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

;; NIX-EMACS-PACKAGE: ormolu
(use-package ormolu
  :defer t
  ;; :config
  )

(provide 'nagy-haskell)
;;; nagy-haskell.el ends here
