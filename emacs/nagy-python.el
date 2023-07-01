;;; nagy-python.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "29.1") python-black hy-mode)
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
  (require 'python-black))

;; (use-package python-black
;;   :after (python)
;;   :config
;;   (map! :map python-mode-map
;;         :localleader
;;         "tb" #'python-black-on-save-mode))

(use-package python
  :pretty 'python-mode
  ("True" . true) ("False" . false))

(provide 'nagy-python)
;;; nagy-python.el ends here
