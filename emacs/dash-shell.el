;;; dash-shell.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "30.1") jq-mode nagy-list nagy-use-package)

(require 'dash)
(require 'jq-mode)

(defun -shell--case (spec)
  (pcase-exhaustive spec
    (`(:json . ,rest)
     (let ((coding-system-for-read 'utf-8)
           (default-directory temporary-file-directory))
       (-shell--case (append (list jq-interactive-command "--sort-keys")
                             rest)))
     )
    (`(,first . ,rest)
     (save-excursion
       (--> (apply #'call-process-region
                   (if (zerop (buffer-size)) "" nil) nil
                   first
                   (if (zerop (buffer-size)) nil t)
                   '(t nil) ; remove stderr
                   nil
                   (--map (format "%s" it)
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

(provide 'dash-shell)
;;; dash-shell.el ends here
