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
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  :bind
  (:map corfu-map
        ("RET" . nil)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<key-chord> f j" . corfu-insert)))

;; (use-package cape)

(provide 'nagy-corfu)
;;; nagy-corfu.el ends here
