;;; dash-shell.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dash nagy-use-package)

(require 'dash)

(defun -shell--case (spec)
  (pcase-exhaustive spec
    (`(:json . ,rest)
     (let ((coding-system-for-read 'utf-8)
           (default-directory temporary-file-directory))
       (-shell--case `("jq" "--sort-keys" ,@rest))))
    (`(,first . ,rest)
     (save-excursion
       (--> (apply #'call-process-region
                   (if (zerop (buffer-size)) "" nil) nil
                   first
                   (if (zerop (buffer-size)) nil t)
                   '(t nil) ; remove stderr
                   nil
                   (--map
                    (cl-typecase it
                      (keyword (if (= 2 (length (symbol-name it)))
                                   (format "-%s" (string-remove-prefix ":" (symbol-name it)))
                                 (format "--%s" (string-remove-prefix ":" (symbol-name it)))))
                      (otherwise (format "%s" it)))
                    rest))
            zerop
            (cl-assert it t "Shell command with non-nil exitcode: %s %S %S" first rest (buffer-string)))))
    ((or (prefix "http://")
         (prefix "https://")
         (prefix "ipfs://")
         )
     (-shell--case (list "curl" "--fail" "--compressed" spec))
     )
    ;; ((prefix "/ipfs/")
    ;;  (-shell--case (format "ipfs://%s" (string-remove-prefix "/ipfs/" spec))))
    ((prefix "rsync://")
     (-shell--case (list "rsync" spec)))
    ((pred stringp)
     (-shell--case (list "sh" "-c" spec)))
    ))

;;;###autoload
(defun -shell (&rest specs)
  (mapcar #'-shell--case specs))

;;;###autoload
(defun -shell1 (&rest specs)
  (-shell--case specs))

;;;###autoload
(defun dollar (spec)
  (-shell--case spec))

;;;###autoload
(defalias '$ (symbol-function 'dollar))

;;;###autoload
(defun dollar-string (spec)
  (with-temp-buffer
    (dollar spec)
    (buffer-string)))

;;;###autoload
(defalias '$s (symbol-function 'dollar-string))

;;;###autoload
(defun dollar-line (spec)
  (with-temp-buffer
    (dollar spec)
    (goto-char (point-min))
    (buffer-substring (point) (line-end-position))))

;;;###autoload
(defalias '$l (symbol-function 'dollar-line))

;;;###autoload
(defun dollar-json (spec)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (dollar spec))
    (goto-char (point-min))
    (json-parse-buffer)))

;;;###autoload
(defalias '$j (symbol-function 'dollar-json))

;;;###autoload
(defun dollar-line2 (&rest spec)
  (with-temp-buffer
    (apply #'dollar2 spec)
    (goto-char (point-min))
    (buffer-substring (point) (line-end-position))))

;;;###autoload
(defalias '$l2 (symbol-function 'dollar-line2))

;;;###autoload
(defun dollar2 (&rest spec)
  (-shell--case spec))

;;;###autoload
(defalias '$2 (symbol-function 'dollar2))

;;;###autoload
(defun dollar-string2 (&rest spec)
  (with-temp-buffer
    (apply #'dollar2 spec)
    (buffer-string)))

;;;###autoload
(defalias '$s2 (symbol-function 'dollar-string2))

;;;###autoload
(defun dollar-json2 (&rest spec)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (apply #'dollar2 spec))
    (goto-char (point-min))
    (json-parse-buffer)))

;;;###autoload
(defalias '$j2 (symbol-function 'dollar-json2))

;;;###autoload
(defun dollar-bytes2 (&rest spec)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (apply #'dollar2 spec)
    (buffer-string)))

;;;###autoload
(defalias '$b2 (symbol-function 'dollar-bytes2))

(provide 'dash-shell)
;;; dash-shell.el ends here
