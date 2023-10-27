;;; nagy-magit.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
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
;; Package-Requires: ((emacs "29.1") magit-section forge general with-editor)
;;
;; This file is NOT part of GNU Emacs.
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
(require 'forge)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'with-editor))

(use-package magit-section
  :general
  (:states 'normal :keymaps 'magit-section-mode-map
           ;; "r" #'magit-section-toggle  ; already rebase in magit itself
           "Ö" #'magit-section-cycle
           "ö" #'magit-section-toggle)
  :bind
  (:map magit-section-mode-map
        ("s-<kp-1>" . magit-section-show-level-1)
        ("s-<kp-2>" . magit-section-show-level-2)
        ("s-<kp-3>" . magit-section-show-level-3)
        ("s-<kp-4>" . magit-section-show-level-4)
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
    (forge-visit-topic (forge-get-topic number))))

(use-package forge
  :bind
  (:map forge-post-mode-map
        ([remap kill-this-buffer] . forge-post-cancel)
        ([remap save-kill-buffer] . forge-post-submit))
  ;; :general
  ;; (:states 'normal :keymaps 'forge-post-mode-map
  ;;          "ö" #'forge-post-submit)
  )

(use-package with-editor
  :bind
  (:map with-editor-mode-map
        ([remap save-kill-buffer] . with-editor-finish)
        ([remap kill-this-buffer] . with-editor-cancel))
  :general
  (:states 'normal :keymaps 'with-editor-mode-map
           "ö" #'with-editor-finish))

(provide 'nagy-magit)
;;; nagy-magit.el ends here
