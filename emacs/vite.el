;;; vite.el --- Vite functions -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") eat nagy-emacs)

(require 'project)
(require 'eat)

;;;###autoload
(defun vite-build ()
  (interactive)
  (with-directory (project-root (project-current))
    (let ((eat-buffer-name "*eat-vite-build*"))
      (when (get-buffer eat-buffer-name)
        (kill-buffer (get-buffer eat-buffer-name)))
      (eat "deno run -A --node-modules-dir npm:vite build"))))

;;;###autoload
(defun vite-dev ()
  (interactive)
  (with-directory (project-root (project-current))
    (let ((eat-buffer-name "*eat-vite-dev*"))
      (if (get-buffer eat-buffer-name)
          (switch-to-buffer eat-buffer-name)
        (eat "deno run -A --node-modules-dir npm:vite dev --clearScreen=false")))))

(provide 'vite)
;;; vite.el ends here
