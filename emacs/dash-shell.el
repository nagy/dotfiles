;;; dash-shell.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dash nagy-use-package)

(require 'dash)
(require 'sh-script)

(defun -shell--case (spec)
  (pcase-exhaustive spec
    (`(:json . ,rest)
     (let ((coding-system-for-read 'utf-8)
           (default-directory temporary-file-directory))
       (-shell--case `("jq" "--sort-keys" ,@rest))))
    (`(,(and (or (prefix "https://")
                 (prefix "http://")
                 (prefix "ipfs://"))
             url))
     (-shell--case (list "curl"
                         "--ipv4"
                         "--fail"
                         "--compressed"
                         url))
     )
    (`(,(and (cl-type url)
             url))
     (-shell--case (list (url-recreate-url url))))
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
                    (flatten-list
                     (remq nil rest))))
            zerop
            (cl-assert it t "Shell command with non-nil exitcode: %s %S %S" first rest (buffer-string)))))
    ))

;;;###autoload
(defun dollar (&rest spec)
  (-shell--case spec))

;;;###autoload
(defun dollar-line (&rest spec)
  (with-temp-buffer
    (apply #'dollar spec)
    (goto-char (point-min))
    (buffer-substring (point) (line-end-position))))

;;;###autoload
(defun dollar-string (&rest spec)
  (with-temp-buffer
    (apply #'dollar spec)
    (buffer-string)))

;;;###autoload
(defun dollar-json (&rest spec)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (apply #'dollar spec))
    (goto-char (point-min))
    (json-parse-buffer)))

;;;###autoload
(defun dollar-bytes (&rest spec)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (apply #'dollar spec)
    (buffer-string)))

(define-obsolete-function-alias 'dollar2 'dollar "now")
(define-obsolete-function-alias 'dollar-line2 'dollar-line "now")
(define-obsolete-function-alias 'dollar-string2 'dollar-string "now")
(define-obsolete-function-alias 'dollar-json2 'dollar-json "now")
(define-obsolete-function-alias 'dollar-bytes2 'dollar-bytes "now")

(provide 'dash-shell)
;;; dash-shell.el ends here
