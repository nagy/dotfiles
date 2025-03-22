;;; nagy-url.el --- url info mode -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") url-knowledge general nagy-use-package)

;; (require 'url-parse)
(require 'general)
;; (require 'nagy-use-package)

(declare-function url-knowledge--get-url-force "url-knowledge")

(defface nagy-url-face
  '((t :inherit (ffap bold dired-header)))
  "my face doc")

(use-package url-knowledge
  :commands (url-knowledge--get-url)
  :preface
  (defun nagy-url-browse-firefox ()
    (interactive)
    (browse-url-firefox (url-knowledge--get-url-force)))
  :general
  (:states 'normal
           "⨏" #'url-knowledge-browse-url
           "⨎" #'nagy-url-browse-firefox
           "»" #'url-knowledge-kill-url))

(provide 'nagy-url)
;;; nagy-url.el ends here
