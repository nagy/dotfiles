;;; nagy-elpher.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

(require 'general)
;; (require 'bookmark)
(declare-function bookmark-prop-get "bookmark")

;; NIX-EMACS-PACKAGE: elpher
(use-package elpher
  :preface
  (defun elpher-bookmark-handler (record)
    (elpher-go (bookmark-prop-get record 'location)))
  (defun elpher-bookmark-make-record ()
    (cons "elpher"
          `((location . ,(url-recreate-url (cadr elpher-current-page)))
            (handler . elpher-bookmark-handler))))
  :commands (elpher-go)
  :functions (elpher-redraw)
  :custom
  (elpher-use-header nil)
  (elpher-ipv4-always t)
  (elpher-connection-timeout 10)
  :config
  (put 'elpher-bookmark-handler 'bookmark-handler-type "Elpher")
  (add-hook 'elpher-mode-hook
            (defun +nagy/elpher-hook ()
              (setq-local bookmark-make-record-function #'elpher-bookmark-make-record)
              ;; (setq-local revert-buffer-function (cmd! (elpher-redraw)))
              ))
  (set-face-attribute 'elpher-gemini-heading1 nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)
  (set-face-attribute 'elpher-gemini-heading2 nil :font "Et Bembo" :height 1.5 :inherit 'modus-themes-heading-2)
  (set-face-attribute 'elpher-gemini-heading3 nil :font "Et Bembo" :height 1.2 :inherit 'modus-themes-heading-3)
  (set-face-attribute 'elpher-gemini-preformatted nil :extend t :inherit 'modus-themes-nuanced-green)
  :general
  (:states 'normal :keymaps 'elpher-mode-map
           "q" #'quit-window
           "M-n" #'elpher-next-link
           "M-p" #'elpher-prev-link)
  (:states 'motion :keymaps 'elpher-mode-map
           ;; (define-key elpher-mode-map "f" nil) ; this might be needed
           "f" #'push-button
           "s" #'elpher-back))

(provide 'nagy-elpher)
;;; nagy-elpher.el ends here
