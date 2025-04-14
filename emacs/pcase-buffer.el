;;; pcase-buffer.el --- My local config -*- lexical-binding: t; -*-

;;;###autoload
(pcase-defmacro buffer (&rest fields)
  (let* ((pcase-buffer--expval (make-symbol "pcase-buffer--expval"))
         (pcase-buffer--val `(cl-typecase ,pcase-buffer--expval
                               (buffer ,pcase-buffer--expval)
                               (string (get-buffer ,pcase-buffer--expval))
                               (process (process-buffer ,pcase-buffer--expval))
                               (window (window-buffer ,pcase-buffer--expval)))))
    `(and ,pcase-buffer--expval
          (guard ,pcase-buffer--expval)
          ,@(mapcar (lambda (field)
                      (pcase-exhaustive field
                        (`(,name ,pat)
                         `(app (lambda (_arg)
                                 (pcase-exhaustive ,name
                                   ('point (with-current-buffer (or ,pcase-buffer--val (current-buffer)) (point)))
                                   ('size (buffer-size ,pcase-buffer--val))
                                   ('hash (buffer-hash ,pcase-buffer--val))
                                   ('name (buffer-name ,pcase-buffer--val))
                                   ('file-name (buffer-file-name ,pcase-buffer--val))
                                   ('process (get-buffer-process ,pcase-buffer--val))
                                   ('buffer (get-buffer (or ,pcase-buffer--val (current-buffer))))
                                   ('window (get-buffer-window ,pcase-buffer--val t))
                                   ('narrowed (with-current-buffer (or ,pcase-buffer--val (current-buffer)) (buffer-narrowed-p)))
                                   ('directory (with-current-buffer (or ,pcase-buffer--val (current-buffer)) default-directory))
                                   ('major-mode (buffer-local-value 'major-mode ,pcase-buffer--val))
                                   ))
                               ,pat))
                        ((pred symbolp)
                         `(app (lambda (_arg)
                                 (pcase-exhaustive ',field
                                   ('point (with-current-buffer (or ,pcase-buffer--val (current-buffer)) (point)))
                                   ('size (buffer-size ,pcase-buffer--val))
                                   ('hash (buffer-hash ,pcase-buffer--val))
                                   ('name (buffer-name ,pcase-buffer--val))
                                   ('file-name (buffer-file-name ,pcase-buffer--val))
                                   ('process (get-buffer-process ,pcase-buffer--val))
                                   ('buffer (get-buffer (or ,pcase-buffer--val (current-buffer))))
                                   ('window (get-buffer-window ,pcase-buffer--val t))
                                   ('narrowed (with-current-buffer (or ,pcase-buffer--val (current-buffer)) (buffer-narrowed-p)))
                                   ('directory (with-current-buffer (or ,pcase-buffer--val (current-buffer)) default-directory))
                                   ('major-mode (buffer-local-value 'major-mode ,pcase-buffer--val))

                                   ))
                               ,field))))
                    fields))))

(provide 'pcase-buffer)
;;; pcase-buffer.el ends here
