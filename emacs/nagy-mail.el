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
  :defer t
  :custom
  (gnus-inhibit-images t)
  ;; (gnus-treat-hide-signature t)
  :config
  ;; fix a bug when attempting to reply. maybe caused by doom?
  (defun gnus-mode-string-quote (&optional string)
    "Quote all \"%\"'s in STRING."
    (string-replace "%" "%%" (or string ""))))

(use-package mu4e
  :bind
  ("<XF86Mail>" . mu4e)
  (:map mu4e-main-mode-map
        ([remap save-kill-buffer] . mu4e-quit)
        ([remap kill-this-buffer] . mu4e-quit)
        ([remap nagy-kill-this-buffer] . mu4e-quit))
  (:map mu4e-headers-mode-map
        ([remap kill-this-buffer] . mu4e-quit)
        ([remap nagy-kill-this-buffer] . mu4e-quit))
  (:map mu4e-view-mode-map
        ("H-j" . mu4e-view-headers-next)
        ("H-k" . mu4e-view-headers-prev))
  (:map mu4e-compose-mode-map
        ("s-z" . message-send-and-exit))
  :general
  (:states 'normal :keymaps 'mu4e-headers-mode-map
           "q" #'mu4e-quit
           "f" #'mu4e-headers-view-message)
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-confirm-quit nil)
  (mu4e-context-policy 'pick-first)
  (mu4e-completing-read-function #'completing-read)
  (mu4e-headers-leave-behavior 'apply)
  (mu4e-update-interval nil)
  (mu4e-get-mail-command "true")
  (mu4e-view-auto-mark-as-read nil)
  (mu4e-change-filenames-when-moving t)
  (mu4e-cache-maildir-list t)
  ;; https://tushartyagi.com/blog/configure-mu4e-and-msmtp/
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (mu4e-headers-fields '(;;(:account . 10)
                         (:mailing-list . 12)
                         (:human-date . 12)
                         (:flags . 6)
                         (:from . 25)
                         (:subject)))
  (mu4e-bookmarks '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                    ("date:today..now" "Today's messages" ?t)
                    ("date:7d..now" "Last 7 days" ?w)
                    ("mime:application/pdf" "Messages with pdfs" ?p)
                    ("maildir:\"/local/\"" "local Inbox" ?L)
                    ("NOT maildir:\"/local/\"" "External Inbox" ?e)))
  ;; ( mu4e-compose-signature user-full-name)
  :config
  (setq mu4e-user-agent-string nil)
  (setq mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
        mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
        mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
        mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
        mu4e-headers-thread-child-prefix         '("├>" . "├▶")
        mu4e-headers-thread-last-child-prefix    '("└>" . "└▶")))

(use-package rmail
  :bind
  (:map rmail-mode-map
        ("H-j" . rmail-next-message)
        ("H-k" . rmail-previous-message)))

(provide 'nagy-mail)
;;; nagy-mail.el ends here
