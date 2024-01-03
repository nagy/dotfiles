;;; nagy-misc2.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: March 03, 2023
;; Modified: March 03, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy-misc2
;; Package-Requires: ((emacs "29.1") reformatter general nagy-use-package)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'reformatter)
(require 'general)

(require 'nagy-use-package)

(use-package conf-mode
  :preface
  (reformatter-define taplofmt
    :group 'conf
    :program "taplo"
    :args `("fmt" "-"))
  :bind
  (:map conf-mode-map
        ("H-j" . forward-paragraph)
        ("H-k" . backward-paragraph))
  :general
  (:states 'normal :keymaps 'conf-toml-mode
           "ö" #'save-buffer)
  :hook
  (conf-toml-mode . taplofmt-on-save-mode)
  :pretty 'conf-toml-mode
  ("true" . true) ("false" . false)
  :cycle 'conf-toml-mode
  ("true" "false"))

(use-package ace-window
  :init
  (setq aw-keys (list ?a ?s ?d ?f ?h ?j ?k ?l ))
  (setq aw-background nil)
  (setq aw-scope 'visible)
  (setq aw-leading-char-style 'path)
  :bind
  ("s-'" . ace-window))

(provide 'nagy-misc2)
;;; nagy-misc.el ends here
