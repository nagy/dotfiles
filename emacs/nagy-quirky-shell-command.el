;;; nagy-quirky-shell-command.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: February 19, 2023
;; Modified: February 19, 2023
;; Keywords:
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(defvar-local nagy-quirky-shell-command--command nil)
(put 'nagy-quirky-shell-command--command 'permanent-local t)

(defun nagy-quirky-shell-command--revert-buffer-function (&rest _ignore)
  (interactive)
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (erase-buffer))
    (insert (shell-command-to-string nagy-quirky-shell-command--command))
    (goto-char (point-min))))

(define-derived-mode nagy-quirky-shell-command-mode text-mode "Quirky-Shell"
  ;; A quirky shell command mode
  ;; We cannot invoke call-process recursively, so we have to use
  ;; (insert (shell-command-to-string ...))
  ;; We cannot reinvoke the mode, because we do not want to loose local variables
  (setq-local revert-buffer-function #'nagy-quirky-shell-command--revert-buffer-function))

(defun nagy-quirky-shell-command (command)
  (interactive "Mquirky-shell-command: ")
  (switch-to-buffer (generate-new-buffer "*quirky*"))
  (nagy-quirky-shell-command-mode)
  (setq nagy-quirky-shell-command--command command)
  (read-only-mode 1)
  (revert-buffer-quick))


(provide 'nagy-quirky-shell-command)
;;; nagy-quirky-shell-command.el ends here
