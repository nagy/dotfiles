;;; nagy-url.el --- url info mode -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") anaphora general url-knowledge nagy-use-package)

(require 'emacs)
(require 'url-knowledge)
(require 'anaphora)
(require 'nagy-use-package)

(defvar-local nagy-url-url nil)
(put 'nagy-url-url 'permanent-local t)

(defface nagy-url-face
  '((t :inherit (ffap bold dired-header)))
  "my face doc")

(use-package url-knowledge
  :preface
  (declare-function evil-global-set-key "evil")
  (defun nagy-url-browse-firefox ()
    (interactive)
    (browse-url-firefox (url-knowledge--get-url)))
  (defun nagy-url-kill-url ()
    (interactive)
    (alet (url-knowledge--get-url)
      (kill-new it)
      (message "Killed: %s" it)))
  (defun nagy-url-kill ()
    (interactive)
    (let* ((url (current-kill 0))
           (parsed (url-generic-parse-url url)))
      (when (url-host parsed)
        (nagy-url-browse-url url))))
  :config
  (evil-global-set-key 'normal "⨏" #'nagy-url-browse-firefox)
  (evil-global-set-key 'normal "»" #'nagy-url-kill-url)
  (keymap-global-set "s-r" #'nagy-url-kill)
  )

(provide 'nagy-url)
;;; nagy-url.el ends here
