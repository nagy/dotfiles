;;; pcase-base64.el --- pcase patterns for base64 encode and decode  -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

(defun pcase-base64--matcher (name field)
  (pcase-exhaustive name
    ('decoded (ignore-errors (base64-decode-string field)))
    ('encoded (base64-encode-string field t))))

;;;###autoload
(pcase-defmacro base64 (&rest fields)
  (let* ((pcase-base64--expval (make-symbol "pcase-base64--expval")))
    `(and ,pcase-base64--expval
          (guard ,pcase-base64--expval)
          ,@(mapcar (lambda (field)
                      (pcase-exhaustive field
                        (`(,name ,pat)
                         `(app (lambda (arg) (pcase-base64--matcher ,name arg)) ,pat))
                        ((pred symbolp)
                         `(app (lambda (arg) (pcase-base64--matcher ',field arg)) ,field))))
                    ;; ((or `(,name ,pat)
                    ;;      (and (pred symbolp) name pat))
                    ;;  `(app (lambda (arg)
                    ;;          (pcase-base64--matcher ,(if (symbolp name) name (list 'quote name)) arg))
                    ;;        ,pat))
                    fields))))

;; (pcase "hello"
;;   ((base64 ('encoded (base64 decoded)))
;;    decoded))

(provide 'pcase-base64)
;;; pcase-base64.el ends here
