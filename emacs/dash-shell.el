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
    ((prefix "/ipfs/")
     (-shell--case (format "ipfs://%s" (string-remove-prefix "/ipfs/" spec))))
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

(defalias '$ (symbol-function 'dollar))

;;;###autoload
(defun dollar-string (spec)
  (with-temp-buffer
    (dollar spec)
    (buffer-string)))

(defalias '$s (symbol-function 'dollar-string))

;;;###autoload
(defun dollar-line (spec)
  (with-temp-buffer
    (dollar spec)
    (goto-char (point-min))
    (buffer-substring (point) (line-end-position))))

(defalias '$l (symbol-function 'dollar-line))

;;;###autoload
(defun dollar-json (spec)
  (with-temp-buffer
    (dollar spec)
    (goto-char (point-min))
    (json-parse-buffer)))

(defalias '$j (symbol-function 'dollar-json))

(provide 'dash-shell)
;;; dash-shell.el ends here
