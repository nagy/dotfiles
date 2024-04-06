;;; nagy-corfu.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "29.1") corfu cape)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package corfu
  :defer t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-quit-at-boundary nil)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  :bind
  (:map corfu-map
        ("RET" . nil)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<key-chord> f j" . corfu-insert)))

(use-package cape
  :general
  (:prefix "M-c"                       ; Choose a particular completion function
           "p" 'completion-at-point
           "t" 'complete-tag            ; etags
           "d" 'cape-dabbrev            ; basically `dabbrev-completion'
           "f" 'cape-file
           "k" 'cape-keyword
           "s" 'cape-elisp-symbol
           "a" 'cape-abbrev
           "i" 'cape-ispell
           "l" 'cape-line
           "w" 'cape-dict
           "\\" 'cape-tex
           "_" 'cape-tex
           "^" 'cape-tex
           "&" 'cape-sgml
           "r" 'cape-rfc1345))

(provide 'nagy-corfu)
;;; nagy-corfu.el ends here
