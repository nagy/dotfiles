;;; nagy-misc.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: March 03, 2023
;; Modified: March 03, 2023
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/nagy/nagy-misc
;; Package-Requires: ((emacs "29.1") nameless golden-ratio)
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
  (require 'nameless))

(use-package nameless
  :diminish nameless-mode
  :hook
  (emacs-lisp-mode . nameless-mode)
  :custom
  (nameless-private-prefix t)
  (nameless-global-aliases
   '(("fl" . "font-lock")
     ("ms" . "magit-section")
     ("ns" . "nix-store"))))

(use-package golden-ratio
  :bind
  ("H-s-=" . golden-ratio-mode))

(provide 'nagy-misc)
;;; nagy-misc.el ends here
