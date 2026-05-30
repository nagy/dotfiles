;;; nagy-passage.el --- Password manager configuration -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-use-package)

(require 'general)

(require 'nagy-use-package nil t)

;; NIX-EMACS-PACKAGE: pass
(use-package pass
  :custom
  (pass-username-field "Username")
  (pass-show-keybindings nil)
  (pass-suppress-confirmations t)
  :config
  ;; override. TODO turn this into advice :replace
  (defun pass-quit ()
    "Kill the buffer quitting the window."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pass-view-mode)
          (kill-buffer buf))))
    (quit-window t))
  ;; silence the header
  (defun pass-display-header ()
    (progn))
  :general
  (:states 'normal :keymaps 'pass-mode-map
           "u" #'pass-copy-url))

;; NIX-EMACS-PACKAGE: password-store
(use-package password-store
  :preface
  (defun nagy-replace-sleep-for-with-sit-for (orig-fun &rest args)
    "Advice that replaces calls to `sleep-for'
with `sit-for'.

This can be sometimes useful if a function is hanging because it
waits for input."
    (cl-letf (((symbol-function 'sleep-for) #'sit-for))
      (apply orig-fun args)))
  :custom
  (password-store-time-before-clipboard-restore 10)
  (password-store-url-field "Url")
  :config
  ;; TODO PR this upstream into the mailing list
  ;; https://github.com/zx2c4/password-store
  ;; https://lists.zx2c4.com/mailman/listinfo/password-store
  (advice-add 'password-store--run :around #'nagy-replace-sleep-for-with-sit-for)
  :bind
  ("M-s-p" . password-store-copy)
  :general
  (:states 'normal :keymaps 'pass-mode-map
           "j" #'pass-next-entry
           "k" #'pass-prev-entry
           "o" #'pass-otp-options
           "w" #'pass-copy
           "b" #'pass-copy-username
           "f" #'pass-view)
  :same "^\\*Password-Store\\*$")

;; NIX-EMACS-PACKAGE: password-store-otp
(use-package password-store-otp
  :defer t
  ;; to add autoloads
  :commands (password-store-otp-token)
  :config
  ;; To remove the "^J" from the line ending
  ;; TODO pr this upstream
  (advice-add 'password-store-otp-token :filter-return #'string-trim-right)
  )

;; NIX-EMACS-PACKAGE: age
(use-package age
  :config
  (age-file-enable)
  )

;; NIX-EMACS-PACKAGE: passage
(use-package passage
  :defer t
  :after age
  :preface
  (declare-function evil-set-initial-state "evil")
  :custom
  (passage-show-keybindings nil)
  (passage-username-field "Username")
  :config
  ;; silence the header
  (defun passage-display-header ()
    (progn))
  ;; temporary until evil-collection mode is created
  (with-eval-after-load 'evil
    (evil-set-initial-state 'passage-mode 'emacs))
  :same "^\\*Passage-Store\\*$"
  :general
  (:states 'normal
           "🔑" #'passage)
  )

(use-package passage-store
  :after age
  :preface
  (defun nagy-passage-store-respect-age-variables (orig-fun &rest args)
    "Advice that sets `age.el' variables as environment variables.
This currently requires that `age-default-identity' and
`age-default-recipient' are strings only."
    (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
      (with-environment-variables
          (("PASSAGE_IDENTITIES_FILE" age-default-identity)
           ("PASSAGE_RECIPIENTS_FILE" age-default-recipient))
        (apply orig-fun args)))
    )
  :config
  (advice-add 'passage-store--run-1 :around #'nagy-passage-store-respect-age-variables)
  )

(use-package passage-store-otp
  :defer t
  :config
  ;; To remove the "^J" from the line ending
  ;; TODO pr this upstream
  (advice-add 'passage-store-otp-token :filter-return #'string-trim-right)
  )

(provide 'nagy-passage)
;;; nagy-passage.el ends here
