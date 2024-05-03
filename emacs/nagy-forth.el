;;; nagy-forth.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "29.1") general forth-mode elforth nagy-use-package)
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

(require 'nagy-use-package)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'forth-mode) ; warns about using cl ;; NIX-IGNORE-WARNINGS
  (require 'elforth)
  )

(use-package forth-mode
  :preface
  (defun nagy-misc2-forth-mode-hook ()
    (setq-local outline-regexp "\\\\ #\\{2,3\\} ")
    (setq-local outline-heading-alist '(("## " . 1) ("### " . 2)))
    (outline-minor-mode 1))
  ;; (defun nagy-forth-send-dot ()
  ;;   (interactive)
  ;;   (comint-send-string (get-buffer-process (current-buffer)) ".\n"))
  :hook
  (forth-mode . nagy-misc2-forth-mode-hook)
  :bind
  ("H-M-4" . forth-mode)
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
  :pretty 'forth-mode
  ("begin" . "→")
  ("again" . "←")
  (":" . "»")
  (":noname" . [?» (Br . Bl) ??])
  (";" . "«")
  ;; elforth
  ("window-buffer" . [?𝒘 (Br . Bl) ?𝒃])
  ("selected-window" . [?𝒔 (Br . Bl) ?𝒘])
  ("buffer-size" . [?𝒃 (Br . Bl) ?𝒔])
  :cycle 'forth-mode
  ("begin" "again")
  :abbrev 'forth-mode
  ("beg" . "begin")
  ("cre" . "create")
  ("var" . "variable")
  ("const" . "constant")
  ("ag" . "again"))

(use-package elforth
  :disabled t
  :general
  (:states 'normal
           "M-¼" #'elforth-eval-region))

(use-package term
  :custom
  (term-buffer-maximum-size 0)
  :config
  (setq serial-speed-history '("115200"))
  :bind
  (:map term-mode-map
        ;; ("H-h" . term-line-mode)  ; does not work
        ("H-l" . term-char-mode))
  (:map term-raw-map
        ("H-h" . term-line-mode)
        ("H-l" . term-char-mode)))

(provide 'nagy-forth)
;;; nagy-forth.el ends here
