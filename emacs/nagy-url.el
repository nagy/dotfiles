;;; nagy-url.el --- url info mode -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") url-knowledge general nagy-use-package)

;; (require 'url-parse)
(require 'general)
;; (require 'nagy-use-package)

(defface nagy-url-face
  '((t :inherit (ffap bold dired-header)))
  "my face doc")

(use-package url-knowledge
  :commands (url-knowledge--get-url)
  :preface
  (defun nagy-url-browse-firefox ()
    (interactive)
    (browse-url-firefox (url-knowledge--get-url)))
  :general
  (:states 'normal
           "⨏" #'url-knowledge-browse-url
           "⨎" #'nagy-url-browse-firefox
           "»" #'url-knowledge-kill-url))

(provide 'nagy-url)
;;; nagy-url.el ends here
