;;; pcase-buffer.el --- My local config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-

(pcase-defmacro buffer (&rest fields)
  (let ((pcase-buffer--expval (gensym "pcase-buffer--expval")))
    `(and ,pcase-buffer--expval
          (guard ,pcase-buffer--expval)
          ,@(mapcar (lambda (field)
                      (pcase-exhaustive field
                        (`(,name ,pat)
                         `(app (lambda (_arg)
                                 (pcase ',name
                                   ('point (with-current-buffer ,pcase-buffer--expval (point)))
                                   ('size (with-current-buffer ,pcase-buffer--expval (buffer-size)))
                                   ('hash (with-current-buffer ,pcase-buffer--expval (buffer-hash)))
                                   ('process (get-buffer-process ,pcase-buffer--expval))))
                               ,pat))
                        ((pred symbolp)
                         `(app (lambda (_arg)
                                 (pcase ',field
                                   ('point (with-current-buffer ,pcase-buffer--expval (point)))
                                   ('size (with-current-buffer ,pcase-buffer--expval (buffer-size)))
                                   ('hash (with-current-buffer ,pcase-buffer--expval (buffer-hash)))
                                   ('process (get-buffer-process ,pcase-buffer--expval))))
                               ,field))))
                    fields))))


(provide 'pcase-buffer)
;;; pcase-buffer.el ends here
