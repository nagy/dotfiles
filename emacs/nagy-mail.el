;;; nagy-mail.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Package-Requires: ((emacs "29.1") mu4e general with-editor)
;;
;;; Commentary:
;;; Code:

(require 'general)

(use-package gnus
  :custom
  (gnus-inhibit-images t)
  ;; (gnus-treat-hide-signature t)
  )

(use-package mu4e
  :bind
  ("<XF86Mail>" . mu4e)
  :custom
  (mu4e-confirm-quit nil)
  (mu4e-context-policy 'pick-first)
  (mu4e-completing-read-function #'completing-read)
  (mu4e-headers-leave-behavior 'apply)
  (mu4e-update-interval nil)
  (mu4e-view-auto-mark-as-read nil)
  ;; ( mu4e-compose-signature user-full-name)
  :config
  (setq mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
        mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
        mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
        mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
        mu4e-headers-thread-child-prefix         '("├>" . "├▶")
        mu4e-headers-thread-last-child-prefix    '("└>" . "└▶")))

(provide 'nagy-mail)
;;; nagy-mail.el ends here
