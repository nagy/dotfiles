;;; nagy-elpher.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
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
;; Package-Requires: ((emacs "29.1") elpher general evil)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)
(require 'bookmark)
(require 'evil)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'elpher))

(use-package elpher
  :preface
  (defun elpher-bookmark-handler (record)
    (elpher-go (bookmark-prop-get record 'location)))
  (defun elpher-bookmark-make-record ()
    (cons "elpher"
          `((location . ,(url-recreate-url (cadr elpher-current-page)))
            (handler . elpher-bookmark-handler))))
  :commands (elpher-go)
  :functions
  elpher-redraw
  :hook
  (elpher-mode . visual-fill-column-mode)
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
           "s" #'elpher-back)
  ;; (evil-define-key 'motion elpher-mode-map (kbd "C-M-h") #'elpher-back)
  ;; (evil-define-key 'motion elpher-mode-map (kbd "C-o") #'elpher-back)
  ;; (evil-define-key 'motion elpher-mode-map (kbd "<C-i>") #'elpher-next-link)
  ;; (evil-define-key 'motion elpher-mode-map (kbd "S-TAB") #'elpher-prev-link)
  ;; (evil-define-key 'motion elpher-mode-map "J" #'elpher-next-link)
  ;; (evil-define-key 'motion elpher-mode-map "K" #'elpher-prev-link)
  ;; (evil-define-key 'motion elpher-mode-map "K" #'elpher-prev-link)
  ;; (evil-define-key 'motion elpher-mode-map "g" nil)
  ;; (evil-define-key 'motion elpher-mode-map "go" #'elpher-go-current)
  ;; (evil-define-key 'motion elpher-mode-map "o" #'nagy-link-hint-open-link)
  ;; (evil-define-key 'normal elpher-mode-map "o" #'nagy-link-hint-open-link)
  ;; (evil-define-key 'normal elpher-mode-map "g" nil)
  )


(provide 'nagy-elpher)
;;; nagy-elpher.el ends here
