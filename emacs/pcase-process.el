;;; pcase-process.el --- My local config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") f)

(require 'f)

(defun pcase-buffer--get-environment (process)
  (string-split (f-read-text (format "/proc/%d/environ" (process-id process)))
                ;; "^ @"
                (format "\x00")
                ))

;;;###autoload
(pcase-defmacro process (&rest fields)
  (let* ((pcase-process--expval (make-symbol "pcase-process--expval"))
         (pcase-process--val `(cl-typecase ,pcase-process--expval
                                (process ,pcase-process--expval)
                                (string (get-process ,pcase-process--expval))
                                (buffer (get-buffer-process ,pcase-process--expval))
                                (window (get-buffer-process (window-buffer ,pcase-process--expval))))))
    `(and ,pcase-process--expval
          (guard ,pcase-process--expval)
          ,@(mapcar (lambda (field)
                      (pcase-exhaustive field
                        (`(,name ,pat)
                         `(app (lambda (_arg)
                                 (pcase-exhaustive ,name
                                   ('name (process-name ,pcase-process--val))
                                   ('id (process-id ,pcase-process--val))
                                   ('plist (process-plist ,pcase-process--val))
                                   ('status (process-status ,pcase-process--val))
                                   ('buffer (process-buffer ,pcase-process--val))
                                   ('filter (process-filter ,pcase-process--val))
                                   ('sentinel (process-sentinel ,pcase-process--val))
                                   ('contact (process-contact ,pcase-process--val))
                                   ('command (process-command ,pcase-process--val))
                                   ('attributes (process-attributes ,pcase-process--val))
                                   ('mark (process-mark ,pcase-process--val))
                                   ('environment (pcase-buffer--get-environment ,pcase-process--val))
                                   ))
                               ,pat))
                        ((pred symbolp)
                         `(app (lambda (_arg)
                                 (pcase-exhaustive ',field
                                   ('name (process-name ,pcase-process--val))
                                   ('id (process-id ,pcase-process--val))
                                   ('plist (process-plist ,pcase-process--val))
                                   ('status (process-status ,pcase-process--val))
                                   ('buffer (process-buffer ,pcase-process--val))
                                   ('filter (process-filter ,pcase-process--val))
                                   ('sentinel (process-sentinel ,pcase-process--val))
                                   ('contact (process-contact ,pcase-process--val))
                                   ('command (process-command ,pcase-process--val))
                                   ('attributes (process-attributes ,pcase-process--val))
                                   ('mark (process-mark ,pcase-process--val))
                                   ('environment (pcase-buffer--get-environment ,pcase-process--val))
                                   ))
                               ,field))))
                    fields))))

(provide 'pcase-process)
;;; pcase-process.el ends here
