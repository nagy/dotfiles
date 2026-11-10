;;; nagy-rust.el --- Rust config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-use-package)

;; NIX-EMACS-PACKAGE: crate
(use-package crate
  :defer t
  :config
  (crate-install-browse-url-handler))

(provide 'nagy-rust)
;;; nagy-rust.el ends here
