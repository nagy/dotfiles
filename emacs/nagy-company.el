;;; nagy-company.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Package-Requires: ((emacs "29.1") company)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'company)

(use-package company
  :diminish 'company-mode
  :bind
  ("C-M-s-¢" . company-mode)
  (:map company-active-map
        ("RET" . nil)
        ("<return>" . nil)
        ("C-l" . company-complete-selection)
        ("<key-chord> f j" . company-complete-selection))
  ;; (:map doom-leader-map
  ;;       ("M-c" . company-mode))
  ;; :config
  ;; (map! :map company-active-map
  ;;       "RET" nil
  ;;       "<return>" nil)
  :config
  (global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  ;; (company-tooltip-align-annotations t)
  (company-tooltip-idle-delay 0.1)
  (company-async-redisplay-delay 0.1)
  (company-dabbrev-downcase nil)
  ;; Only search the current buffer for `company-dabbrev' (a backend that
  ;; suggests text your open buffers). This prevents Company from causing
  ;; lag once you have a lot of buffers open.
  (company-dabbrev-other-buffers nil)
  )

(provide 'nagy-company)
;;; nagy-company.el ends here
