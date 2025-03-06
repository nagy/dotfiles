;;; nagy-common-lisp.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "30.1") sly link-hint general nagy-use-package)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)
;; (require 'nagy-use-package)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'sly)
  (sly--setup-contribs))

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
  (add-to-list 'browse-url-handlers
               '("^file:///nix/store/.*hyperspec.*/Body/" . my-browser-url-eww-hyperspec))
  :same
  "^\\*hyperspec\\*"
  :config
  ;; nix-build "<nixos>" -A nur.repos.nagy.hyperspec --no-out-link
  (setq common-lisp-hyperspec-root "file:///nix/store/2hli5955grxkbyqp2vzzdnl556rn0bkz-hyperspec-7.0/share/HyperSpec/"))

(use-package sly
  :custom
  (sly-db-focus-debugger t)
  (sly-description-autofocus t)
  (inferior-lisp-program "sbcl")
  :same
  (rx bos "*sly-" (or "inspector"
                      "mrepl"
                      "description"
                      "macroexpansion"))
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
  ("rfs" . "read-from-string"))

(use-package sly-mrepl
  :defer t
  :preface
  (defun nagy-common-lisp-sly-mrepl-return ()
    (interactive)
    (sly-mrepl-return)
    (goto-char (point-max)))
  :bind
  (:map sly-mrepl-mode-map
        ("H-ö" . nagy-common-lisp-sly-mrepl-return)
        ("H-r" . sly-mrepl-clear-repl)
        ("M-ð" . sly-disassemble-symbol)
        ("<key-chord> f j" . sly-mrepl-return))
  :general
  (:states 'normal :keymaps 'sly-mrepl-mode-map
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
  ("list" . list))

;; https://github.com/joaotavora/sly/issues/334
;; (setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil) ; this can also go into "~/.slynkrc"
;; (setf slynk::*inspector-slots-default-order* :unsorted)
;; (setf slynk::*inspector-slots-default-order* :unsorted)
;; (setf slynk::*inspector-slots-default-grouping* :inheritance)
;; display reuse window
;; (defadvice! +sly-mrepl-return-refresh-inspector (&rest args)
;;   :after #'sly-mrepl-return
;;   (when (get-buffer "*sly-inspector for sbcl*")
;;     (with-current-buffer (get-buffer "*sly-inspector for sbcl*")
;;       (revert-buffer))))

;;; Scheme

(use-package scheme
  :bind
  (:map scheme-mode-map
        ("s-." . scheme-send-last-sexp)
        ("s--" . scheme-send-definition))
  :hook
  (scheme-mode . lispy-mode)
  :pretty 'scheme-mode
  ("define" . setq)
  ("if" . if)
  ("import" . import))

(use-package link-hint
  :bind
  ("H-a" . link-hint-open-link))

(provide 'nagy-common-lisp)
;;; nagy-common-lisp.el ends here
