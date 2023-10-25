;;; nagy-python.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "29.1") python-black reformatter hy-mode general nagy-use-package)
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

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'python-black))

(use-package python
  :preface
  (reformatter-define ruff-format
    :group 'python
    :program "ruff"                     ; needs ruff >= 0.1.2
    :args `("format" "--stdin-filename" ,input-file "-"))
  :hook
  (python-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-format-on-save-mode)
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  :pretty 'python-mode
  ("True" . true) ("False" . false)
  ("def" . def)
  ("class" . class)
  ("class" . defclass)
  ("raise" . throw)
  ("import" . import)
  ("try" . try) ("except" . except)
  ("return" . return)
  ("pass" . "…")
  :bind
  ("H-M-p" . python-mode)
  :general
  (:states 'normal
           "þ" #'run-python))

(provide 'nagy-python)
;;; nagy-python.el ends here
