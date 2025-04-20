;;; nagy-list.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Nagy
;;
;; Package-Requires: ((emacs "30.1") dash anaphora nagy-emacs)
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
  suffix column-names format-cell column-width prepare)
(cl-defun nagy-list-make (name &key suffix column-names format-cell column-width prepare)
  (add-to-list 'auto-mode-alist `(,(format "%s\\.json\\'" (string-replace "." "\\." suffix))
                                  . nagy-list-mode))
  (setf (alist-get name nagy-list--known-configs nil nil #'equal)
        (nagy-list--make-config :suffix suffix
                                :column-names column-names
                                :format-cell format-cell
                                :column-width column-width
                                :prepare prepare)))

(defvar-local nagy-list--sym nil)
(put 'nagy-list--sym 'permanent-local t)
(cl-defgeneric nagy-list-sym2 (obj)
  nil)
(defun nagy-list-sym ()
  (or nagy-list--sym
      (setq nagy-list--sym
            (or (nagy-list-sym2 nagy-list--data)
                (nagy-list-buffer-file-name-sym)))))
(defvar nagy-list-table-default-column-width 30)
(defvar-local nagy-list-buffer-file-name nil)
(put 'nagy-list-buffer-file-name 'permanent-local t)

(defvar-local nagy-list--data nil)
(put 'nagy-list--data 'permanent-local t)
(defvar-local nagy-list--beforebody nil)
(put 'nagy-list--beforebody 'permanent-local t)
(defun nagy-list--data ()
  (or (gather nagy-list--data)
      (setq nagy-list--data
            (json-parse-string nagy-list--beforebody
                               ;; :object-type 'alist
                               ;; :array-type 'list
                               ))))

(defvar-local nagy-list--id-sym "id")
(put 'nagy-list--id-sym 'permanent-local t)

(defun nagy-list-column-names ()
  (awhen (nagy-list-config-column-names (or (map-elt nagy-list--known-configs (nagy-list-sym))
                                            (map-elt nagy-list--known-configs (symbol-name (nagy-list-sym)))))
    (funcall it)))

(defun nagy-list-prepare ()
  (awhen (nagy-list-config-prepare (or (map-elt nagy-list--known-configs (nagy-list-sym))
                                       (map-elt nagy-list--known-configs (symbol-name (nagy-list-sym)))))
    (funcall it)))

(defun nagy-list-format-cell (column value)
  (aif (nagy-list-config-format-cell (or (map-elt nagy-list--known-configs (nagy-list-sym))
                                         (map-elt nagy-list--known-configs (symbol-name (nagy-list-sym)))))
      (funcall it column value)
    value))

(defun nagy-list-column-width (column)
  (aif (nagy-list-config-column-width (alist-get (nagy-list-sym) nagy-list--known-configs))
      (funcall it column)
    nagy-list-table-default-column-width))

(defun nagy-list-alist-get-deep (alist keys)
  "deepable version of alist-get.
That means, KEY can also be a cons."
  (cl-typecase keys
    (symbol (or (map-elt alist keys)
                (map-elt alist (symbol-name keys))))
    (cons (while keys
            (setq alist (or (map-elt alist (pop keys)))))
          alist)))

(defun nagy-list--format-1 (value &optional prevalue)
  (pcase value
    ;; Some predefined formats
    (:date (if prevalue
               (propertize
                (or (ignore-errors (format-time-string "%F/%R"
                                                       (if (stringp prevalue)
                                                           (encode-time (iso8601-parse prevalue))
                                                         (when (numberp prevalue)
                                                           (seconds-to-time prevalue)))))
                    "")
                'font-lock-face 'org-date)
             (nagy-list--format-1 :null)))
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
    (:null (propertize "-" 'font-lock-face 'parenthesis))
    (:false (propertize "false" 'font-lock-face 'modus-themes-fg-red-intense))
    ((pred (eq t _)) (propertize "true" 'font-lock-face 'modus-themes-fg-green-intense))
    ((pred null) nil)
    ((pred proper-list-p)
     (mapconcat (lambda (x)
                  (if (and (stringp x)
                           (string-prefix-p "/home/" x))
                      (abbreviate-file-name x)
                    (nagy-list--format-1 x)
                    ))
                value
                ", "))
    ((pred vectorp)
     (nagy-list--format-1 (seq-into value 'list)))
    ((pred stringp)
     ;; (propertize (string-replace "\n" "" value) 'font-lock-face 'font-lock-string-face)
     (string-replace "\n" "" value)
     )
    ((pred processp)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (process-live-p value) 'modus-themes-fg-green-cooler 'modus-themes-fg-red-cooler)))
    ((pred bufferp)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (buffer-live-p value) 'modus-themes-fg-green-cooler 'modus-themes-fg-red-cooler)))
    ((pred markerp)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (buffer-live-p (marker-buffer value)) 'modus-themes-fg-green-cooler 'modus-themes-fg-red-cooler)))
    ((pred windowp)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (window-live-p value) 'modus-themes-fg-green-cooler 'modus-themes-fg-red-cooler)))
    ((pred framep)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (frame-live-p value) 'modus-themes-fg-green-cooler 'modus-themes-fg-red-cooler)))
    ((pred keywordp)
     (propertize (symbol-name value)
                 'font-lock-face 'font-lock-builtin-face))
    ((pred numberp)
     (propertize (number-to-string value)
                 ;; 'font-lock-face 'font-lock-number-face
                 'font-lock-face 'marginalia-number
                 ))
    (_ (string-replace "\n" "" (prin1-to-string value)))
    ;; (_ (format "%s" value))
    ))

(defun nagy-list--revert-hook ()
  ;; (when (eq major-mode 'nagy-list-mode))
  (when buffer-file-name
    (ungather nagy-list--data)
    (setq nagy-list--data nil)
    (setq nagy-list--beforebody (let ((bn buffer-file-name))
                                  (with-temp-buffer
                                    (insert-file-contents-literally bn)
                                    (buffer-string))))
    (revert-buffer--default nil t)))

(defun nagy-list-table-entries ()
  (seq-map (lambda (obj)
             (list
              (or (map-elt obj nagy-list--id-sym) (map-elt obj 'id) (map-elt obj "id"))
              `[,@(mapcar (lambda (it)
                            (propertize (let* ((prevalue (nagy-list-alist-get-deep obj it))
                                               (value (nagy-list-format-cell it prevalue)))
                                          (or (nagy-list--format-1 value prevalue)
                                              (nagy-list--format-1 prevalue)
                                              ""))
                                        'nagy-list--data obj))
                          (nagy-list-column-names))])
             )
           (nagy-list--data)
           ))

(defun nagy-list--data-at-point ()
  (get-text-property (point) 'nagy-list--data))

(defun nagy-list-from-eval (expr)
  (interactive "XEval: ")
  (switch-to-buffer (generate-new-buffer (format "Nagy-list-eval: %s" (type-of expr))))
  (setq nagy-list--data expr)
  (nagy-list-mode)
  (current-buffer))
(keymap-global-set "H-s-L" #'nagy-list-from-eval)

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
    t ;; return non-nil, signalling it has been written
    ))
(add-hook 'write-file-functions #'nagy-list--write-file-function)

;;;###autoload
(define-derived-mode nagy-list-mode tabulated-list-mode "nagy-list"
  (setq nagy-list--beforebody (buffer-substring-no-properties (point-min) (point-max)))
  (add-hook 'change-major-mode-hook #'nagy-list--change-major-mode-hook nil t)
  (add-hook 'tabulated-list-revert-hook #'nagy-list--revert-hook nil t)
  ;; Used by auto-revert
  ;; (setq-local buffer-stale-function (lambda (&optional _noconfirm)
  ;;                                     (and
  ;;                                      ;; If the buffer is handled magically
  ;;                                      (not (unhandled-file-name-directory buffer-file-name))
  ;;                                      ;; and visible, then mark it stale
  ;;                                      (get-buffer-window nil t))
  ;;                                     ;; t
  ;;                                     ))
  ;; (setq-local auto-revert-interval 5)
  (setq tabulated-list-format
        `[,@(--map `(,(propertize (capitalize (cl-etypecase it
                                                (symbol (symbol-name it))
                                                (string it)
                                                (cons (symbol-name (car (last it))))))
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
