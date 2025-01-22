;;; nagy-list.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Package-Requires: ((emacs "29.1") dash anaphora general nagy-emacs)
;;
;;; Commentary:
;;; Code:

(require 'subr-x)
(require 'nagy-emacs)

(eval-when-compile
  (require 'dired))
(declare-function dired-get-marked-files "dired")
;; (defvar dired-directory-face)
(require 'dash)
(require 'general)
(declare-function iso8601-parse "iso8601")

(defvar nagy-list--known-configs nil)
(cl-defstruct (nagy-list-config (:constructor nagy-list--make-config))
  suffix
  column-names
  format-cell
  column-width
  prepare
  )
(cl-defun nagy-list-make (name &key
                               suffix
                               column-names
                               format-cell
                               column-width
                               prepare)
  (let ((config (nagy-list--make-config
                 :suffix suffix
                 :column-names column-names
                 :format-cell format-cell
                 :column-width column-width
                 :prepare prepare)))
    (add-to-list 'auto-mode-alist `(,(format "%s\\.json\\'" suffix) . nagy-list-mode))
    (prog1 config
      (setf (alist-get name nagy-list--known-configs nil nil #'equal) config))))

(defvar nagy-list-table-default-column-width 30)
(defvar-local nagy-list-buffer-file-name nil)
(put 'nagy-list-buffer-file-name 'permanent-local t)
(defvar-local nagy-list-sym nil)
(put 'nagy-list-sym 'permanent-local t)
(defvar-local nagy-list--data nil)
(put 'nagy-list--data 'permanent-local t)
(defvar nagy-list-testers nil)

(defvar-local nagy-list--beforebody nil)
(put 'nagy-list--beforebody 'permanent-local t)

(defvar-local nagy-list--id-sym 'id)
(put 'nagy-list--id-sym 'permanent-local t)

(defun nagy-list-column-names ()
  (awhen (nagy-list-config-column-names (alist-get nagy-list-sym nagy-list--known-configs))
    (funcall it)))

(defun nagy-list-prepare ()
  (awhen (nagy-list-config-prepare (alist-get nagy-list-sym nagy-list--known-configs))
    (funcall it)))

(defun nagy-list-format-cell (column value)
  (aif (nagy-list-config-format-cell (alist-get nagy-list-sym nagy-list--known-configs))
      (funcall it column value)
    value))

(defun nagy-list-column-width (column)
  (aif (nagy-list-config-column-width (alist-get nagy-list-sym nagy-list--known-configs))
      (funcall it column)
    nagy-list-table-default-column-width))

(defun nagy-list-alist-get-deep (alist keys)
  "deepable version of alist-get.
That means, KEY can also be a cons."
  (cl-typecase keys
    (symbol (alist-get keys alist))
    (cons (while keys
            (setq alist (alist-get (pop keys) alist)))
          alist)))

(defun nagy-list--format-1 (value &optional prevalue)
  (pcase value
    ;; Some predefined formats
    (:date (propertize
            (or (ignore-errors (format-time-string "%F/%R"
                                                   (if (stringp prevalue)
                                                       (encode-time (iso8601-parse prevalue))
                                                     (when (numberp prevalue)
                                                       (seconds-to-time prevalue)))))
                "")
            'font-lock-face 'org-date))
    (:path (propertize (abbreviate-file-name prevalue) 'font-lock-face dired-directory-face))
    (:bytes (propertize (file-size-human-readable prevalue)
                        'font-lock-face
                        (cond ((> prevalue (* 512 1024 1024 1024)) ; > 512GB
                               'modus-themes-fg-red-intense)
                              ((> prevalue (* 1024 1024 1024)) ; > 1GB
                               'modus-themes-fg-red-faint)
                              ((> prevalue (* 512 1024 1024)) ; > 512MB
                               'modus-themes-fg-yellow-intense)
                              ((> prevalue (* 1024 1024)) ; > 1MB
                               'modus-themes-fg-yellow-faint)
                              ((> prevalue (* 512 1024))  ; > 512KB
                               'modus-themes-fg-green-intense)
                              (t 'modus-themes-fg-green-faint)) ))
    (:identifier (propertize (format "%s" prevalue) 'font-lock-face 'magit-hash))
    ((pred null) nil)
    ((pred listp) (propertize (mapconcat (lambda (x)
                                           (if (and (stringp x)
                                                    (string-prefix-p "/home/" x))
                                               (abbreviate-file-name x)
                                             (format "%s" x)))
                                         value
                                         ", ")
                              'font-lock-face dired-directory-face))
    ((pred stringp) (string-replace "\n" "" value))
    ((pred numberp) (propertize (number-to-string value) 'font-lock-face 'marginalia-number))
    (_ (format "%s" value))))


(defun nagy-list-table-entries ()
  (cl-loop for obj in nagy-list--data
           collect
           `(,(alist-get nagy-list--id-sym obj)
             [,@(--map
                 (propertize (let* ((prevalue (nagy-list-alist-get-deep obj it))
                                    (value (nagy-list-format-cell it prevalue)))
                               (or (nagy-list--format-1 value prevalue)
                                   (nagy-list--format-1 prevalue)
                                   ""))
                             'nagy-list--data obj)
                 (nagy-list-column-names))])))

(defun nagy-list--data-at-point ()
  (get-text-property (point) 'nagy-list--data))

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
         (save-match-data (split-string it "\\."))
         last
         car
         intern)))

(defun nagy-list--write-file-function ()
  (when nagy-list--beforebody
    (write-region nagy-list--beforebody nil buffer-file-name)
    (set-buffer-modified-p nil)
    t) ;; return non-nil, signalling it has been written
  )
(add-hook 'write-file-functions #'nagy-list--write-file-function)

(define-derived-mode nagy-list-mode tabulated-list-mode "nagy-list"
  (setq nagy-list--beforebody (buffer-substring-no-properties (point-min) (point-max)))
  (add-hook 'change-major-mode-hook #'nagy-list--change-major-mode-hook nil t)
  (setq nagy-list-sym (nagy-list-buffer-file-name-sym))
  (when (> (buffer-size) 0)
    (goto-char (point-min))
    (setq nagy-list--data
          (json-parse-buffer :object-type 'alist
                             :array-type 'list))
    (setq nagy-list-sym
          (let ((res (nagy-list-buffer-file-name-sym)))
            (pcase-dolist  (`(,x . ,y) nagy-list-testers)
              (ignore-errors
                (when (funcall y (car nagy-list--data))
                  (setq res x))))
            res))
    (with-silent-modifications
      (erase-buffer)))
  ;; (unless nagy-list--data)
  (setq nagy-list--data (let ((str (format "%s" nagy-list--beforebody)))
                          (with-temp-buffer
                            (save-excursion (insert str))
                            (json-parse-buffer :object-type 'alist
                                               :array-type 'list))))
  (setq tabulated-list-format
        `[,@(--map `(,(propertize (capitalize (symbol-name
                                               (cl-etypecase it
                                                 (symbol it)
                                                 (cons (car (last it))))))
                                  'face 'bold)
                     ,(or (nagy-list-column-width it) nagy-list-table-default-column-width)
                     t)
                   (nagy-list-column-names)) ])
  (setq tabulated-list-entries #'nagy-list-table-entries)
  (nagy-list-prepare)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (set-buffer-modified-p nil)
  (read-only-mode 1))
(keymap-global-set "H-M-L" #'nagy-list-mode)

(defun nagy-list-table-dired-find-file (filename)
  (interactive (list (car (dired-get-marked-files t))))
  (switch-to-buffer (create-file-buffer filename))
  (setq nagy-list-buffer-file-name filename)
  (nagy-list-mode))

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "H-," #'nagy-list-table-dired-find-file))

(defun nagy-list--change-major-mode-hook ()
  (when nagy-list--beforebody
    (with-silent-modifications
      (save-excursion
        (erase-buffer)
        (insert nagy-list--beforebody)))
    (setq nagy-list--beforebody nil)))

(defun nagy-list-sort-by-time ()
  (interactive)
  (aif (-elem-index 'time (nagy-list-column-names))
      (progn (tabulated-list-sort it)
             (tabulated-list-sort it)
             (goto-char (point-min)))
    (user-error "No 'time column found")))

(provide 'nagy-list)
;;; nagy-list.el ends here
