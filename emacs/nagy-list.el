;;; nagy-list.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Package-Requires: ((emacs "29.1") f s dash anaphora general)
;;
;;; Commentary:
;;; Code:

(require 'anaphora)
(require 'general)
(require 'subr-x)
(require 's)
(require 'f)
(require 'dash)

(defvar nagy-list-table-default-column-width 40)
(defvar-local nagy-data-table-obj nil)
(put 'nagy-data-table-obj 'permanent-local t)

(cl-defgeneric nagy-list-column-names ()
  '())

(cl-defgeneric nagy-list-format-cell (column value)
  (ignore column)
  (format "%s" value))

(cl-defgeneric nagy-list-column-width (column)
  (ignore column)
  nagy-list-table-default-column-width)

(defun tabulated-list-current-column-number ()
  "Return the current column number in the tabulated list."
  (declare (side-effect-free t))
  (cl-loop with sum = 0
           for width in (mapcar 'cadr tabulated-list-format)
           for i from 0
           do (cl-incf sum width)
           when (<= (current-column) sum)
           return i))

(defun tabulated-list-kill-ring-save ()
  (interactive)
  (aprog1 (substring-no-properties (elt (tabulated-list-get-entry (point))
                                        (tabulated-list-current-column-number)))
    (kill-new it)
    (when (called-interactively-p 'interactive)
      (message "%s" it))))

(use-package tabulated-list
  :bind
  (:map tabulated-list-mode-map
        ("M-w" . tabulated-list-kill-ring-save)))

(defvar-local nagy-list-buffer-file-name nil)
(put 'nagy-list-buffer-file-name 'permanent-local t)
(defvar-local nagy-list-sym nil)
(put 'nagy-list-sym 'permanent-local t)

(cl-defun nagy-list-buffer-file-name-sym (&optional (filename (or nagy-list-buffer-file-name buffer-file-name "")))
  (when (string-suffix-p ".json" filename)
    (->> filename
         (string-remove-suffix ".json")
         (s-split "\\.")
         last
         car
         intern)))

(cl-defun nagy-list-table-syms (&optional (filename (or nagy-list-buffer-file-name buffer-file-name "")))
  (when (string-suffix-p ".json" filename)
    (nagy-list-column-names)))

(define-derived-mode nagy-list-mode tabulated-list-mode "nagy-list"
  (setq nagy-list-sym (nagy-list-buffer-file-name-sym))
  (setq tabulated-list-format
        (cl-loop for s in (nagy-list-table-syms)
                 vconcat
                 (vector (list (symbol-name s)
                               (nagy-list-column-width s)
                               t))))
  (setq tabulated-list-entries
        (cl-loop for obj in (json-parse-string nagy-data-table-obj
                                               :object-type 'alist
                                               :array-type 'list)
                 collect
                 `(,(alist-get 'id obj)
                   ,(cl-loop for key in (nagy-list-table-syms)
                             vconcat
                             (vector (nagy-list-format-cell key
                                                            (alist-get key obj)))))))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (set-buffer-modified-p nil))

(defun nagy-list-table-dired-find-file ()
  (interactive)
  (let* ((file (car (dired-get-marked-files t)))
         (str (f-read-text file)))
    (with-current-buffer (create-file-buffer file)
      (setq nagy-list-buffer-file-name file)
      (setq nagy-data-table-obj str)
      (nagy-list-mode)
      ;; (setq buffer-file-name file)
      (switch-to-buffer (current-buffer)))))

(require 'dired)
(keymap-set dired-mode-map "H-," #'nagy-list-table-dired-find-file)

(provide 'nagy-list)
;;; nagy-list.el ends here
