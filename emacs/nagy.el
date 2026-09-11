;;; nagy.el --- -*- lexical-binding: t; -*-

(defvar nagy-do-not-load nil
  "Features deliberately not auto-loaded.
Modules are discovered by scanning `load-path' for `nagy-*.el'
files; anything listed here is skipped.")

(dolist (dir load-path)
  (when (string-search "nagy" (file-name-nondirectory (directory-file-name dir)))
    (dolist (file (ignore-errors
                    (directory-files dir nil "\\`nagy-.*\\.el\\'")))
      (let ((feature (intern (file-name-sans-extension file))))
        (unless (memq feature nagy-do-not-load)
          (require feature nil (not init-file-debug)))))))

(provide 'nagy)
;;; nagy.el ends here
