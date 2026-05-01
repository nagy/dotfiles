;;; nagy-list.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dash anaphora nagy-emacs)

(require 'subr-x)
(require 'nagy-emacs)

(eval-when-compile
  (require 'dired))
(declare-function dired-get-marked-files "dired")
;; (defvar dired-directory-face)
;; (require 'dash)
;; (require 'general)
(declare-function iso8601-parse "iso8601")

(defvar-local nagy-list--data nil)
(put 'nagy-list--data 'permanent-local t)
(defvar-local nagy-list--beforebody nil)
(put 'nagy-list--beforebody 'permanent-local t)

(defun nagy-list--data ()
  (or (alet (gather nagy-list--data)
        (if (and (stringp it)
                 (string-empty-p it))
            nil
          (if it it)))
      (setq nagy-list--data (json-parse-string nagy-list--beforebody))))

(defvar-local nagy-list--columns nil)
(put 'nagy-list--columns 'permanent-local t)

(defun nagy-list-format-cell (column value)
  (aif (--> (map-elt nagy-list--columns column)
            (seq-elt it 1))
      (cl-typecase it
        (function (funcall it value))
        ;; (symbol (funcall (symbol-function it) value))
        (t it))
    value))

(defun nagy-list-column-width (column)
  (--> (map-elt nagy-list--columns column)
       (seq-elt it 0)
       ;; Fallback to default
       (or it 30)))

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
    (:bytes
     (if (stringp prevalue) (setq prevalue (string-to-number prevalue)))
     (propertize (file-size-human-readable prevalue)
                 'font-lock-face
                 (cond ((> prevalue (* 512 1024 1024 1024)) ; > 512GB
                        'nagy-fg-red-intense)
                       ((> prevalue (* 1024 1024 1024)) ; > 1GB
                        'nagy-fg-red-faint)
                       ((> prevalue (* 512 1024 1024)) ; > 512MB
                        'nagy-fg-yellow-intense)
                       ((> prevalue (* 1024 1024)) ; > 1MB
                        'nagy-fg-yellow-faint)
                       ((> prevalue (* 512 1024)) ; > 512KB
                        'nagy-fg-green-intense)
                       (t 'nagy-fg-green-faint)) ))
    (:identifier (propertize (format "%s" prevalue) 'font-lock-face 'magit-hash))
    (:null (propertize "-" 'font-lock-face 'parenthesis))
    (:false (propertize "false" 'font-lock-face 'nagy-fg-red-intense))
    ((pred (eq t _)) (propertize "true" 'font-lock-face 'nagy-fg-green-intense))
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
    (:milliseconds
     (if (stringp prevalue) (setq prevalue (string-to-number prevalue)))
     (setq prevalue
           (format-time-string "%Y-%m-%dT%T+00:00" (seconds-to-time (/ prevalue 1000.0))))
     (nagy-list--format-1 :date prevalue))
    ((pred vectorp)
     (nagy-list--format-1 (seq-into value 'list)))
    ((pred stringp)
     ;; (propertize (string-replace "\n" "" value) 'font-lock-face 'font-lock-string-face)
     (string-replace "\n" "" value)
     )
    ((pred processp)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (process-live-p value) 'success 'error)))
    ((pred bufferp)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (buffer-live-p value) 'success 'error)))
    ((pred markerp)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (buffer-live-p (marker-buffer value)) 'success 'error)))
    ((pred windowp)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (window-live-p value) 'success 'error)))
    ((pred framep)
     (propertize (prin1-to-string value)
                 'font-lock-face (if (frame-live-p value) 'success 'error)))
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

(defun nagy-list-table-entries ()
  (seq-map (lambda (obj)
             (list
              (ignore-errors (or (map-elt obj 'id)
                                 (map-elt obj "id")))
              `[,@(mapcar (lambda (it)
                            (propertize (let* ((prevalue (nagy-list-alist-get-deep obj it))
                                               (value (nagy-list-format-cell it prevalue)))
                                          (or (nagy-list--format-1 value prevalue)
                                              (nagy-list--format-1 prevalue)
                                              ""))
                                        'nagy-list--data obj))
                          (map-keys nagy-list--columns))]))
           nagy-list--data))

(defun nagy-list--data-at-point ()
  (get-text-property (point) 'nagy-list--data))

(cl-defun nagy-list-buffer-file-name-sym (&optional (filename (or buffer-file-name "")))
  (when (or (string-suffix-p ".json" filename)
            (string-suffix-p ".json.zst" filename))
    (--> filename
         (string-remove-suffix ".zst" it)
         (string-remove-suffix ".json" it)
         (save-match-data (split-string it "\\."))
         last
         car
         intern)))

;;;###autoload
(define-derived-mode nagy-list-mode tabulated-list-mode "nagy-list"
  (setq nagy-list--beforebody (buffer-string))
  (cl-assert (seqp nagy-list--data))
  ;; (add-hook 'tabulated-list-revert-hook #'nagy-list--revert-hook nil t)
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
                     ,(nagy-list-column-width it)
                     t)
                   (map-keys nagy-list--columns)) ])
  (setq tabulated-list-entries #'nagy-list-table-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (set-buffer-modified-p nil)
  (read-only-mode 1)
  )
(keymap-global-set "H-M-L" #'nagy-list-mode)

(provide 'nagy-list)
;;; nagy-list.el ends here
