;;; nagy-hledger.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") embark anaphora s dash general nagy-use-package)

(require 'dash)
(require 'anaphora)
(require 'embark)
(require 's)

;; NIX-EMACS-PACKAGE: hledger-mode
(use-package hledger-mode
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands (hledger-backward-entry
             hledger-pulse-momentary-current-entry
             hledger-next-or-new-entry)
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))
  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))
  ;; :config
  ;; (evil-set-initial-state 'hledger-view-mode 'emacs)
  :bind (("C-c j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
  )

(declare-function hledger-get-accounts "hledger-reports")
(defun embark-target-hledger-account-at-point ()
  (awhen (member (thing-at-point 'filename t)
                    (hledger-get-accounts))
    (-let* (((beg . end) (cons (car (bounds-of-thing-at-point 'filename))
                               (cdr (bounds-of-thing-at-point 'filename))))
            )
      `(account ,(car it) ,beg . ,end)
      )))
(add-to-list 'embark-target-finders #'embark-target-hledger-account-at-point)

(defvar-keymap embark-account-map
  :doc "Keymap for Embark account actions."
  :parent embark-general-map
  "RET" #'hledger-message-account-bal)
(add-to-list 'embark-keymap-alist '(account embark-account-map) nil #'equal)

(defun hledger-message-account-bal (account)
  (interactive "MAccount:")
  (message "%S"
   (shell-command-to-string (s-lex-format "hledger bal ${account}")))
  )

(provide 'nagy-hledger)
;;; nagy-hledger.el ends here
