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
  :config
  (defun my-browser-url-eww-hyperspec (url &optional _new-window)
    (interactive (browse-url-interactive-arg "URL: "))
    (switch-to-buffer (get-buffer-create "*hyperspec*"))
    (eww-mode)
    (setq-local shr-inhibit-images t)
    (eww url)
    ;; (evil-scroll-line-down 7)
    )
  (push '("^file:///nix/store/.*hyperspec.*/Body/" . my-browser-url-eww-hyperspec)
        browse-url-handlers)
  :same
  "^\\*hypserspec\\*"
  :custom
  ;; nix-build "<nixos>" -A nur.repos.nagy.hyperspec --no-out-link
  (common-lisp-hyperspec-root "file:///nix/store/2hli5955grxkbyqp2vzzdnl556rn0bkz-hyperspec-7.0/share/HyperSpec/"))

(use-package sly
  :custom
  (sly-db-focus-debugger t)
  (sly-description-autofocus t)
  (inferior-lisp-program "sbcl")
  :same
  "^\\*sly-inspector "
  ;; "^\\*sly-mrepl "
  ;; "^\\*sly-description\\*"
  ;; "^\\*sly-macroexpansion"
  ;;   :hook
  ;;   (sly-inspector-mode . visual-line-mode)
  ;;   (sly-db-mode . visual-line-mode)
  :general
  (:states 'normal :keymaps 'lisp-mode-map
           "ö" #'sly-eval-defun)
  (:states 'normal :keymaps 'sly-inspector-mode-map
           "o" #'link-hint-open-link)
  :bind
  (:map lisp-mode-map
        ("s-." . sly-eval-last-expression)
        ("s-:" . sly-eval-defun))
  :abbrev 'lisp-mode
  ("opt" . "optimize")
  ("decl" . "declare")
  ("spe" . "speed")
  ("saf" . "safety")
  ("la" . "lambda")
  ("req" . "require")
  ("rfs" . "read-from-string")
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
           "ð" #'sly-disassemble-symbol
           "µ" #'sly-expand-1-inplace
           "Ö" #'sly-mrepl-previous-input-or-button
           "C-ö" #'sly-mrepl-next-input-or-button
           "ö" #'nagy-common-lisp-sly-mrepl-return)
  :abbrev 'sly-mrepl-mode
  ("disa" . "disassemble")
  ("la" . "lambda")
  ("req" . "require")
  ("rfs" . "read-from-string")
  ;; :pretty 'sly-mrepl-mode ; does not work, probably needs fontification
  ;; ("lambda" . lambda)
  )

(use-package lisp-mode
  :bind
  ("H-M-l" . lisp-mode)
  :pretty 'lisp-mode
  ("list" . list)
  )


;;; Scheme

(use-package scheme
  :bind
  (:map scheme-mode-map
        ("s-." . scheme-send-last-sexp)
        ("s--" . scheme-send-definition)))

(provide 'nagy-common-lisp)
;;; nagy-common-lisp.el ends here
