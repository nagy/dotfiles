;;; nagy-common-lisp.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "29.1") sly link-hint general nagy-use-package)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)
(require 'nagy-use-package)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'sly)
  )

(use-package hyperspec
  :disabled
  :load-path (lambda () (concat doom-local-dir "/straight/repos/sly/lib/"))
  ;; :commands (hyperspec-lookup)
  :custom
  ;; nix-build "<nixos>" -A nur.repos.nagy.hyperspec --no-out-link
  (common-lisp-hyperspec-root "file:///nix/store/2hli5955grxkbyqp2vzzdnl556rn0bkz-hyperspec-7.0/share/HyperSpec/"))

(use-package sly
  :custom
  (sly-db-focus-debugger t)
  (sly-description-autofocus t)
  :same
  "^\\*sly-inspector "
  :general
  (:states 'normal :keymaps 'lisp-mode-map
           "ö" #'sly-eval-defun)
  (:states 'normal :keymaps 'sly-inspector-mode-map
           "o" #'link-hint-open-link)
)

(use-package sly-mrepl
  :disabled
  :preface
  (defun nagy-common-lisp-sly-mrepl-return ()
    (interactive)
    (sly-mrepl-return)
    (evil-goto-line nil))
  :defer t
  :bind
  (:map sly-mrepl-mode-map
        ("H-r" . sly-mrepl-clear-repl))
  :general
  (:states 'normal :keymaps 'sly-mrepl-mode-map
           "ö" #'nagy-common-lisp-sly-mrepl-return))

;; (use-package hyperspec
;;   :load-path (lambda () (concat doom-local-dir "/straight/repos/sly/lib/"))
;;   :commands (hyperspec-lookup)
;;   :bind
;;   (:map doom-leader-map
;;         ("alh" . hyperspec-lookup)
;;         ("a H-h" . hyperspec-lookup))
;;   :init
;;   ;; nix-build "<nixos>" -A nur.repos.nagy.hyperspec --no-out-link
;;   (setq common-lisp-hyperspec-root "file:///nix/store/2hli5955grxkbyqp2vzzdnl556rn0bkz-hyperspec-7.0/share/HyperSpec/"))

;;; Scheme

(use-package scheme
  :bind
  (:map scheme-mode-map
        ("s-." . scheme-send-last-sexp)
        ("s--" . scheme-send-definition)))

(provide 'nagy-common-lisp)
;;; nagy-common-lisp.el ends here
