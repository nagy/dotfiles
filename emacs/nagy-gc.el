;;; nagy-gc.el --- Description -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "30.1") with-editor)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'general)

;; More info
;; https://news.ycombinator.com/item?id=15802409
;; https://akrl.sdf.org/#org1ce771c

(defun GC-DISABLE ()
  (interactive)
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 1.0)
  ;; (setq garbage-collection-messages t)
  (garbage-collect)
  (fset 'garbage-collect #'ignore)
  ;; (run-with-idle-timer 60 t #'real-garbage-collect)
  )

(defvar real-garbage-collect (symbol-function 'garbage-collect))

(use-package emacs
  :preface
  (defun real-garbage-collect ()
    (interactive)
    (setq values nil)                   ; cleanup references
    (clear-image-cache t)
    (prog1 (funcall real-garbage-collect)
      (malloc-trim 0)))
  :config
  (run-with-idle-timer 60 t #'real-garbage-collect)
  :general
  (:states 'normal
           "🗑" #'real-garbage-collect))

(defun real-memory-report ()
  (interactive)
  (cl-letf (((symbol-function 'garbage-collect) #'real-garbage-collect))
    (memory-report)))

(provide 'nagy-gc)
;;; nagy-gc.el ends here
