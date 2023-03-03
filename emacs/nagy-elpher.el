;;; nagy-elpher.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: January 02, 2023
;; Modified: January 02, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy-elpher
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)
(require 'bookmark)
(require 'evil)
(defvar doom-leader-map)

(use-package elpher
  :commands (elpher-bookmark-handler elpher-go elpher-bookmark-make-record)
  :functions
  elpher-redraw
  :hook
  (elpher-mode . visual-fill-column-mode)
  :bind
  (:map doom-leader-map
        ("ae" . elpher))
  :general
  (:states 'normal :keymaps 'elpher-mode-map
           "q" #'quit-window
           "M-n" #'elpher-next-link
           "M-p" #'elpher-prev-link)
  :custom
  (elpher-use-header nil)
  (elpher-ipv4-always t)
  (elpher-connection-timeout 10)
  :config
  (put 'elpher-bookmark-handler 'bookmark-handler-type "Elpher")
  (defun elpher-bookmark-handler (record)
    (elpher-go (bookmark-prop-get record 'location)))
  (defun elpher-bookmark-make-record ()
    (cons "elpher"
          `((location . ,(url-recreate-url (cadr elpher-current-page)))
            (handler . elpher-bookmark-handler))))
  (add-hook 'elpher-mode-hook
            (defun +nagy/elpher-hook ()
              (setq-local bookmark-make-record-function #'elpher-bookmark-make-record)
              ;; (setq-local revert-buffer-function (cmd! (elpher-redraw)))
              ))
  (define-key elpher-mode-map "f" nil)
  (evil-define-key 'motion elpher-mode-map "f" #'push-button)
  (evil-define-key 'motion elpher-mode-map "s" #'elpher-back)
  (evil-define-key 'motion elpher-mode-map (kbd "C-M-h") #'elpher-back)
  (evil-define-key 'motion elpher-mode-map (kbd "C-o") #'elpher-back)
  (evil-define-key 'motion elpher-mode-map (kbd "<C-i>") #'elpher-next-link)
  (evil-define-key 'motion elpher-mode-map (kbd "S-TAB") #'elpher-prev-link)
  (evil-define-key 'motion elpher-mode-map "J" #'elpher-next-link)
  (evil-define-key 'motion elpher-mode-map "K" #'elpher-prev-link)
  (evil-define-key 'motion elpher-mode-map "K" #'elpher-prev-link)
  (evil-define-key 'motion elpher-mode-map "g" nil)
  (evil-define-key 'motion elpher-mode-map "go" #'elpher-go-current)
  (evil-define-key 'motion elpher-mode-map "o" #'nagy-link-hint-open-link)
  (evil-define-key 'normal elpher-mode-map "o" #'nagy-link-hint-open-link)
  (evil-define-key 'normal elpher-mode-map "g" nil)
  (set-face-attribute 'elpher-gemini-heading1 nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)
  (set-face-attribute 'elpher-gemini-heading2 nil :font "Et Bembo" :height 1.5 :inherit 'modus-themes-heading-2)
  (set-face-attribute 'elpher-gemini-heading3 nil :font "Et Bembo" :height 1.2 :inherit 'modus-themes-heading-3)
  (set-face-attribute 'elpher-gemini-preformatted nil :extend t :inherit 'modus-themes-nuanced-green))


(provide 'nagy-elpher)
;;; nagy-elpher.el ends here
