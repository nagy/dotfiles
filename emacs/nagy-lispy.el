;;; nagy-lispy.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: December 01, 2022
;; Modified: December 01, 2022
;; Version: 0.0.1
;; Keywords: extensions
;; Homepage: https://github.com/nagy/nagy-magit
;; Package-Requires: ((emacs "29.1") lispy lispyville general nagy-use-package)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)

(use-package lispy
  :diminish 'lispy-mode
  :commands (lispy-mode)
  :custom
  (lispy-completion-method 'default)
  ;; :config
  ;; Obscures key-chord
  ;; (map! :map lispy-mode-map
  ;;       "f" nil  ;; was special-lispy-flow
  ;;       )
  :hook
  (emacs-lisp-mode . lispy-mode)
  :bind
  ("s-(" . lispy-mode)
  ("s-)" . lispyville-mode)
  (:map lispy-mode-map
        ("H-x" . lispy-kill-at-point))
  (:map lispy-mode-map-lispy
        ("]" . nil)
        ("[" . nil)
        ("C-," . nil)  ; was lispy-kill-at-point
        ))

(use-package lispyville
  :diminish 'lispyville-mode
  :bind
  (:map lispyville-mode-map
        ("C-M-S-o" . lispy-oneline)
        ("M-S-RET" . lispy-multiline))
  :hook
  (lispy-mode . lispyville-mode)
  :general
  (:states 'normal :keymaps 'lispyville-mode-map
           "g C-j" #'lispy-down
           "g C-k" #'lispy-up
           ";" #'lispy-comment
           "ł" #'lispyville-backward-atom-end
           "€" #'lispyville-backward-atom-end
           "¶" #'lispyville-forward-atom-end))

(provide 'nagy-lispy)
;;; nagy-lispy.el ends here
