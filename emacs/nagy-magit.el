;;; nagy-magit.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: December 01, 2022
;; Modified: December 01, 2022
;; Version: 0.0.1
;; Keywords: extensions
;; Homepage: https://github.com/nagy/nagy-magit
;; Package-Requires: ((emacs "29.1") magit-section forge general)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'bookmark)
(require 'eieio)                        ; because of 'oref
(require 'magit)
(require 'general)
(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'forge))

(use-package magit-section
  :general
  (:states 'normal :keymaps 'magit-section-mode-map
           "Ö" #'magit-section-cycle
           "ö" #'magit-section-toggle)
  :bind
  (:map magit-section-mode-map
        ("C-ö" . magit-section-cycle-global)
        ("H-j" . magit-section-forward)
        ("H-k" . magit-section-backward)))


(defun nagy-magit--forge--make-bookmark ()
  (let ((bookmark (cons nil (bookmark-make-record-default 'no-file))))
    (bookmark-prop-set bookmark 'handler  'nagy-magit--forge--handle-bookmark)
    (bookmark-prop-set bookmark 'mode     major-mode)
    (bookmark-prop-set bookmark 'filename (magit-toplevel))
    (bookmark-prop-set bookmark 'forge-topic-number (oref forge-buffer-topic number))
    (bookmark-prop-set bookmark 'defaults (list (magit-bookmark-name)))
    bookmark))

(defun nagy-magit--forge--handle-bookmark (bookmark)
  (let ((default-directory (bookmark-get-filename bookmark))
        (number (bookmark-prop-get bookmark 'forge-topic-number)))
    (forge-visit (forge-get-topic number))))

(use-package forge
  :bind
  (:map forge-post-mode-map
        ([remap kill-this-buffer] . forge-post-cancel)
        ([remap save-kill-buffer] . forge-post-submit)))

(provide 'nagy-magit)
;;; nagy-magit.el ends here
