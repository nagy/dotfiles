;;; nagy-corfu.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "29.1") corfu)
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
  (require 'corfu))

(use-package corfu
  :defer t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (corfu-auto-prefix 0))

(provide 'nagy-corfu)
;;; nagy-corfu.el ends here
