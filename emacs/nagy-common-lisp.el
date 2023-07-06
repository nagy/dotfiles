;;; nagy-common-lisp.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "29.1") sly)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'sly)
  )

(use-package sly
  :config)

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

(use-package scheme
  :bind
  (:map scheme-mode-map
        ("s-." . scheme-send-last-sexp)
        ("s--" . scheme-send-definition)))

(provide 'nagy-common-lisp)
;;; nagy-common-lisp.el ends here
