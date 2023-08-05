;;; nagy-forth.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "29.1") general forth-mode elforth)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'comint)

(require 'general)
(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'forth-mode) ; warns about using cl ;; NIX-IGNORE-WARNINGS
  (require 'elforth)
  )

(use-package forth-mode
  :config
  (defun nagy-forth-send-dot ()
    (interactive)
    (comint-send-string (get-buffer-process (current-buffer)) ".\n"))
  ;; (map! :map forth-interaction-mode-map
  ;;       "H-." #'nagy-forth-send-dot
  ;;       :n "." #'nagy-forth-send-dot)
  ;; (defun forth-eval-buffer ()
  ;;   (interactive)
  ;;   (forth-eval (buffer-substring-no-properties (point-min) (point-max))))
  ;; (map! :map forth-mode-map
  ;;       "C-:" #'forth-eval
  ;;       :n "Ö" #'forth-eval-buffer
  ;;       :n "↑" #'forth-eval-buffer
  ;;       :n "ö" #'forth-eval-defun)
  )

(use-package elforth
  :disabled t
  :general
  (:states 'normal
           "M-¼" #'elforth-eval-region))

(provide 'nagy-forth)
;;; nagy-forth.el ends here
