;;; xwininfo-mode.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

(defvar exwm--id)

(define-derived-mode xwininfo-mode text-mode "xwininfo"
  ;; (with-silent-modifications
  ;;   (insert "hello\n"))
  )

(defun xwininfo-from-buffer (theid)
  (interactive (list exwm--id))
  (switch-to-buffer (generate-new-buffer (format "◫Wininfo:0x%X"  theid)))
  (xwininfo-mode)
  (with-silent-modifications
    (save-excursion
      (call-process "xwininfo" nil t nil "-tree" "-id" (format "0x%X" theid))
      (insert (make-separator-line) "\n")
      (call-process "xprop" nil t nil "-id" (format "0x%X" theid))))
  (setq-local header-line-format
              `("%b " ,(format "0x%X" theid))))

(provide 'xwininfo-mode)
;;; xwininfo-mode.el ends here
