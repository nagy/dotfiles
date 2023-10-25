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

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'company))

(use-package company
  :diminish 'company-mode
  :defer t
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
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-tooltip-idle-delay 0.1)
  (company-async-redisplay-delay 0.1))

(provide 'nagy-company)
;;; nagy-company.el ends here
