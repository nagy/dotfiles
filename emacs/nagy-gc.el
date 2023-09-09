;;; nagy-gc.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
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
;; Package-Requires: ((emacs "29.1") general with-editor)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun GC-ENABLE ()
  "Gc tame https://akrl.sdf.org/#org1ce771c ."
  (interactive)
  ;; Set garbage collection threshold to 1GB.
  (setq gc-cons-threshold #x40000000)      ; somehow this does not apply, it gets set back by somebody else
  (setq gc-cons-threshold #x10000000)
  ;; (setq garbage-collection-messages t)
  ;; (defadvice! +timed-gargabe-collect (orig-fn &rest args)
  ;;   :around #'garbage-collect
  ;;   (message "Garbage Collector has run for %.06fsec"
  ;;            (k-time
  ;;             (apply orig-fn args))))
  ;; When idle for 15sec run the GC no matter what.
  (defvar k-gc-timer
    (run-with-idle-timer 15 t
                         (lambda ()
                           (garbage-collect)
                           ;; (message "Garbage Collector has run for %.06fsec"
                           ;;   (k-time (garbage-collect)))
                           ))))

(defun timed-garbage-collect ()
  (interactive)
  (message "(garbage-collect) took: %f seconds" (k-time (garbage-collect))))

;; (map! :n "⏢" #'timed-garbage-collect)
;; (map! :n "⏢" #'garbage-collect)

;; https://news.ycombinator.com/item?id=15802409
;; ;; never collect.
;;  (setq gc-cons-threshold 10000000000)
;;  (defun garbage-collect (&rest args)
;;    (message "trying to garbage collect. probably you want to quit emacs."))
;;  (setq garbage-collection-messages t)


(provide 'nagy-gc)
;;; nagy-gc.el ends here
