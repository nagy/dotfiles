;;; nagy-use-package.el --- use-package configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: February 11, 2023
;; Modified: February 11, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy-use-package
;; Package-Requires: ((emacs "30.1") dash diminish)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'dash)

;;; display-buffer-alist helpers
;;;###autoload
(defun nagy-same-window (regexp)
  "Display buffers with a name that matches REGEXP in the current window.

Add a `display-buffer-alist' entry for REGEXP.  Newest entry wins, because
`add-to-list' prepends by default."
  (add-to-list 'display-buffer-alist `(,regexp display-buffer-same-window)))

;; also try `switch-to-buffer-obey-display-actions'. Does not solve it completely.
(defun nagy-replace-switch-to-buffer-other-window (orig-fun &rest args)
  "Advice that replaces calls to `switch-to-buffer-other-window'
with `switch-to-buffer'."
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (apply orig-fun args)))

(defun nagy-replace-split-window-sensibly (orig-fun &rest args)
  "Advice that replaces calls to `split-window-sensibly'
with `ignore'."
  (cl-letf (((symbol-function 'split-window-sensibly) #'ignore))
    (apply orig-fun args)))

;;;###autoload
(pcase-defmacro suffix (suffix)
  "Pattern (suffix SUFFIX) matches if the string ends with SUFFIX."
  `(and (pred stringp)
        (pred (string-suffix-p ,suffix))))

;;;###autoload
(pcase-defmacro prefix (prefix)
  "Pattern (prefix PREFIX) matches if the string starts with PREFIX."
  `(and (pred stringp)
        (pred (string-prefix-p ,prefix))))

;;;###autoload
(pcase-defmacro infix (infix)
  "Pattern (infix INFIX) matches if the string contains INFIX."
  `(and (pred stringp)
        (pred (s-contains-p ,infix))))

;;;###autoload
(pcase-defmacro derived (&rest modes)
  "Pattern (derived MODES...)."
  `(pred (lambda (mode-or-buffer)
           (cl-typecase mode-or-buffer
             (buffer (provided-mode-derived-p
                      (buffer-local-value 'major-mode mode-or-buffer) ,@modes))
             (string (provided-mode-derived-p
                      (buffer-local-value 'major-mode (get-buffer mode-or-buffer)) ,@modes))
             (symbol (provided-mode-derived-p mode-or-buffer ,@modes))))))

(provide 'nagy-use-package)
;;; nagy-use-package.el ends here
