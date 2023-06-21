;;; nagy-company.el --- Description -*- lexical-binding: t; -*-
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
  :defer t
  :bind
  ("C-M-s-¢" . company-mode)
  (:map company-active-map
        ("C-l" . company-complete-selection))
  ;; (:map doom-leader-map
  ;;       ("M-c" . company-mode))
  :custom
  (company-idle-delay 0.25)
  (company-minimum-prefix-length 1)
  (company-tooltip-idle-delay 0.25)
  (company-async-redisplay-delay 0.1))

(provide 'nagy-company)
;;; nagy-company.el ends here
