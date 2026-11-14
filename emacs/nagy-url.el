;;; nagy-url.el --- url info mode -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") url-knowledge nagy-use-package)

(require 'general)

(declare-function url-knowledge--get-url-force "url-knowledge")

(defface nagy-url-face
  '((t :inherit (ffap bold dired-header)))
  "my face doc"
  :group 'emacs)

(use-package url-knowledge
  :commands (url-knowledge--get-url)
  :preface
  (defun nagy-url-browse-firefox ()
    (interactive)
    (browse-url-firefox (url-knowledge--get-url)))
  :general
  (:states 'normal
           "⨏" #'url-knowledge-browse-url
           "⨎" #'nagy-url-browse-firefox
           "»" #'url-knowledge-kill-url))

;; NIX-EMACS-PACKAGE: buffer-url
(use-package buffer-url
  :defer t)

;;; Buffer URL and file lists

;; This section is inactive on purpose.  Uncomment the whole block to
;; use it.  `url-to-filename3' maps one URL to one local path.  That
;; helper is not part of this file.

;; (require 'cl-lib)
;; (eval-when-compile (require 'subr-x))

;; (defun list-urls ()
;;   "Show one URL for each line of the current buffer.
;; The URL comes from `url-knowledge--get-url-force'.  The help text
;; of each entry gives the line number in the source buffer."
;;   (interactive)
;;   (let* ((source (buffer-name))
;;          (entries
;;           (save-excursion
;;             (goto-char (point-min))
;;             (let (result)
;;               (while (not (eobp))
;;                 (when-let* ((url (ignore-errors
;;                                  (url-knowledge--get-url-force))))
;;                   (push (cons url (line-number-at-pos)) result))
;;                 (forward-line 1))
;;               (nreverse result)))))
;;     (unless entries
;;       (user-error "No URLs in buffer %s" source))
;;     (let ((buffer (get-buffer-create "*URLs*")))
;;       (with-current-buffer buffer
;;         (let ((inhibit-read-only t))
;;           (erase-buffer)
;;           (text-mode)
;;           (dolist (entry entries)
;;             (insert (propertize (car entry)
;;                                 'help-echo (format "%s, line %d"
;;                                                    source (cdr entry)))
;;                     "\n"))
;;           (goto-char (point-min))))
;;       (switch-to-buffer buffer))))

;; (defun list-files ()
;;   "Show the local files of the URLs in the current buffer.
;; Take each URL at the start of a line, map it to a local path with
;; `url-to-filename3', then list the files in that directory that
;; start with that path.  Visit the result with Dired."
;;   (interactive)
;;   (let (urls files)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (when-let* ((url (thing-at-point 'url)))
;;           (push url urls))
;;         (forward-line 1)))
;;     (setq urls (nreverse urls))
;;     (dolist (url urls)
;;       (when-let* ((filename (url-to-filename3 url)))
;;         (let* ((filename (string-remove-suffix "%2F" filename))
;;                (directory (or (file-name-directory filename)
;;                               default-directory))
;;                (prefix (regexp-quote (file-name-nondirectory filename))))
;;           (setq files (nconc files
;;                              (directory-files directory t
;;                                               (concat "\\`" prefix)))))))
;;     (unless files
;;       (user-error "No local files for %d URLs" (length urls)))
;;     (dired (cons (generate-new-buffer-name "*Files*") files))))

;; Buffer key chords.  Add these maps after their major modes load.
;; (dolist (map '(text-mode-map prog-mode-map tabulated-list-mode-map
;;                magit-mode-map dired-mode-map))
;;   (when (boundp map)
;;     (keymap-set (symbol-value map) "<normal-state> <key-chord> F U"
;;                 #'list-urls)
;;     (keymap-set (symbol-value map) "<normal-state> <key-chord> F I"
;;                 #'consult-keep-lines)
;;     (keymap-set (symbol-value map) "<normal-state> <key-chord> F F"
;;                 #'list-files)))
;; (evil-global-set-key 'motion (kbd "g ↓") #'list-urls)

(provide 'nagy-url)
;;; nagy-url.el ends here
