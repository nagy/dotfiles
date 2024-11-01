;;; nagy-list.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Package-Requires: ((emacs "29.1") f s dash anaphora general)
;;
;;; Commentary:
;;; Code:

(require 'subr-x)

(require 'anaphora)
(require 'dash)
(require 'f)
(require 'general)
(require 's)

(defvar nagy-list-table-default-column-width 30)
(defvar-local nagy-list-buffer-file-name nil)
(put 'nagy-list-buffer-file-name 'permanent-local t)
(defvar-local nagy-list-sym nil)
(put 'nagy-list-sym 'permanent-local t)

(cl-defgeneric nagy-list-column-names ()
  '())

(cl-defgeneric nagy-list-format-cell (column value)
  (ignore column)
  (format "%s" value))

(cl-defgeneric nagy-list-column-width (column)
  (ignore column)
  nagy-list-table-default-column-width)

(cl-defgeneric nagy-list-table-entries ()
  (cl-loop for obj in (json-parse-string (f-read-text nagy-list-buffer-file-name)
                                         :object-type 'alist
                                         :array-type 'list)
           collect
           `(,(alist-get 'id obj)
             [,@(--map (nagy-list-format-cell it (or (alist-get it obj)
                                                     (alist-get it (alist-get 'metadata obj))))
                       (nagy-list-column-names))])))

(defun tabulated-list-column-number-at-point ()
  "Return the column number in the tabulated list at POS."
  (declare (side-effect-free t))
  (cl-loop with sum = 0
           for width in (mapcar 'cadr tabulated-list-format)
           for i from 0
           do (cl-incf sum width)
           when (<= (current-column) sum)
           return i))

(defun tabulated-list-kill-ring-save ()
  (interactive)
  (aprog1 (substring-no-properties (elt (tabulated-list-get-entry)
                                        (tabulated-list-column-number-at-point)))
    (kill-new it)
    (when (called-interactively-p 'interactive)
      (message "Copied %S" it))))

(use-package tabulated-list
  :bind
  (:map tabulated-list-mode-map
        ("M-w" . tabulated-list-kill-ring-save)))

(cl-defun nagy-list-buffer-file-name-sym (&optional (filename (or nagy-list-buffer-file-name buffer-file-name "")))
  (when (or (string-suffix-p ".json" filename)
            (string-suffix-p ".json.zst" filename))
    (--> filename
         (string-remove-suffix ".zst" it)
         (string-remove-suffix ".json" it)
         (s-split "\\." it)
         last
         car
         intern)))

(define-derived-mode nagy-list-mode tabulated-list-mode "nagy-list"
  (setq nagy-list-sym (nagy-list-buffer-file-name-sym))
  (setq tabulated-list-format
        `[,@(--map `(,(propertize (capitalize (symbol-name it))
                                  'face 'bold)
                     ,(nagy-list-column-width it) t)
                   (nagy-list-column-names)) ])
  (setq tabulated-list-entries #'nagy-list-table-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (set-buffer-modified-p nil))

(defun nagy-list-table-dired-find-file ()
  (interactive)
  (let ((file (car (dired-get-marked-files t))))
    (with-current-buffer (create-file-buffer file)
      (setq nagy-list-buffer-file-name file)
      (nagy-list-mode)
      ;; (setq buffer-file-name file)
      (switch-to-buffer (current-buffer)))))

(require 'dired)
(keymap-set dired-mode-map "H-," #'nagy-list-table-dired-find-file)

(defun nagy-list-sort-by-time ()
  (interactive)
  (aif (-elem-index 'time (nagy-list-column-names))
      (progn (tabulated-list-sort it)
             (tabulated-list-sort it)
             (goto-char (point-min)))
    (user-error "No 'time column found")))

(provide 'nagy-list)
;;; nagy-list.el ends here
