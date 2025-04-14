;;; pcase-url.el --- My local config -*- lexical-binding: t;  -*-

;; todo set autoload here

(require 'url-parse)

;;;###autoload
(pcase-defmacro url (&rest fields)
  "Parses EXPVAL according to `url-generic-parse-url'.

Accepts either a string or an already parsed url object as EXPVAL."
  (let ((pcase-url--expval (make-symbol "pcase-url--expval"))
        (pcase-url-- (make-symbol "pcase-url--")))
    `(and ,pcase-url--expval
          (let ,pcase-url--
            (cl-typecase ,pcase-url--expval
              ;; (buffer (aand (buffer-local-value 'nagy-url-url ,pcase-url--expval) (url-generic-parse-url it)))
              (url ,pcase-url--expval)
              (string (url-generic-parse-url ,pcase-url--expval))))
          (guard ,pcase-url--) ;; the above let should not be nil
          ,@(mapcar (lambda (field)
                      (pcase-exhaustive field
                        (`(,name ,pat)
                         `(app (lambda (_arg) (oref ,pcase-url-- ,name)) ,pat))
                        ((pred symbolp)
                         `(app (lambda (_arg) (oref ,pcase-url-- ,field)) ,field))))
                    fields))))

(provide 'pcase-url)
;;; pcase-url.el ends here
