;;; pcase-base64.el --- My local config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") dash)

(require 'dash)

(defun pcase-base64--matcher (name field)
  (pcase-exhaustive name
    ('decoded (ignore-errors (base64-decode-string field)))
    ('encoded (base64-encode-string field t))))

;;;###autoload
(pcase-defmacro base64 (&rest fields)
  (let* ((pcase-base64--expval (gensym "pcase-base64--expval")))
    `(and ,pcase-base64--expval
          (guard ,pcase-base64--expval)
          ,@(--map (pcase-exhaustive it
                     (`(,name ,pat)
                      `(app (lambda (arg) (pcase-base64--matcher ,name arg)) ,pat))
                     ((pred symbolp)
                      `(app (lambda (arg) (pcase-base64--matcher ',it arg)) ,it))
                     ;; ((or `(,name ,pat)
                     ;;      (and (pred symbolp) name pat))
                     ;;  `(app (lambda (arg)
                     ;;          (pcase-base64--matcher ,(if (symbolp name) name (list 'quote name)) arg))
                     ;;        ,pat))
                     )
                   fields))))

;; (pcase "hello"
;;   ((base64 ('encoded (base64 decoded)))
;;    decoded))

(provide 'pcase-base64)
;;; pcase-base64.el ends here
