;;; pcase-window.el --- My local config -*- lexical-binding: t; -*-

;;;###autoload
(pcase-defmacro window (&rest fields)
  (let ((pcase-window--expval (gensym "pcase-window--expval")))
    `(and ,pcase-window--expval
          (guard ,pcase-window--expval)
          ,@(mapcar (lambda (field)
                      (pcase-exhaustive field
                        (`(,name ,pat)
                         `(app (lambda (_arg)
                                 (pcase-exhaustive ,name
                                   ('buffer (window-buffer ,pcase-window--expval))
                                   ('frame (window-frame ,pcase-window--expval))
                                   ('width (window-width ,pcase-window--expval))
                                   ('height (window-height ,pcase-window--expval))
                                   ('left (window-left ,pcase-window--expval))
                                   ('right (window-right ,pcase-window--expval))
                                   ('parameters (window-parameters ,pcase-window--expval))
                                   ))
                               ,pat))
                        ((pred symbolp)
                         `(app (lambda (_arg)
                                 (pcase-exhaustive ',field
                                   ('buffer (window-buffer ,pcase-window--expval))
                                   ('frame (window-frame ,pcase-window--expval))
                                   ('width (window-width ,pcase-window--expval))
                                   ('height (window-height ,pcase-window--expval))
                                   ('left (window-left ,pcase-window--expval))
                                   ('right (window-right ,pcase-window--expval))
                                   ('parameters (window-parameters ,pcase-window--expval))
                                   ))
                               ,field))))
                    fields))))

(provide 'pcase-window)
;;; pcase-window.el ends here
