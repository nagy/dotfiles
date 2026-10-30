;;; nagy-mode-line.el --- Mode line configuration -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") memoize)

;; NIX-EMACS-PACKAGE: memoize
(require 'memoize)

;;; Commentary:

;; Custom mode line.  The right block shows the JSON value at the buffer
;; start and at point, the nagy-list types, the file size and the buffer
;; process.  Everything after `mode-line-format-right-align' renders
;; flush with the right edge of the window.
;;
;; `mode-line-buffer-identification' cannot carry the buffer name here,
;; because Dired overrides it.  The `%b' element in `mode-line-format'
;; does that job instead.

;;; Code:

(defvar-local nagy-mode-line--jsvar nil
  "Short description of the JSON value at `point-min', or nil.")

(defvar-local nagy-mode-line--jsvar-point nil
  "Short description of the JSON value at point, or nil.")

(defvar-local nagy-mode-line--jsvar-timer nil
  "Pending badge refresh timer of this buffer, or nil.")

(defvar-local nagy-mode-line--jsvar-seen-point nil
  "Point position of the last refresh attempt, or nil.")

(defun nagy-mode-line--jsvar-calc (&optional position)
  "Return a short description of the JSON value at POSITION.
POSITION defaults to point.  Return nil when this buffer does not
hold small enough valid JSON."
  (when (and (derived-mode-p 'js-json-mode)
             large-file-warning-threshold
             (< (buffer-size) large-file-warning-threshold))
    (when-let* ((it (ignore-errors
                      (save-excursion
                        (goto-char (or position (point)))
                        (json-parse-buffer)))))
      (pcase (type-of it)
        ('string (format "S%d" (length it)))
        ('hash-table (format "H%d" (hash-table-count it)))
        ('integer (format "N%d" it))
        ('float (format "F%f" it))
        ('symbol (format "%S" it))
        ('vector (format "A%d" (length it)))
        ('cons (format "L%d" (length it)))
        (_ (format "%S" it))))))

(defun nagy-mode-line--jsvar-refresh ()
  "Recompute both JSON badges of the current buffer."
  (setq nagy-mode-line--jsvar-seen-point (point)
        nagy-mode-line--jsvar (when-let* ((it (nagy-mode-line--jsvar-calc (point-min))))
                                (propertize it 'face 'nerd-icons-lsilver))
        nagy-mode-line--jsvar-point (when-let* ((it (nagy-mode-line--jsvar-calc)))
                                      (propertize it 'face 'button)))
  (force-mode-line-update))

(defun nagy-mode-line--jsvar-schedule (&rest _)
  "Refresh the JSON badges of this buffer after the next editing pause."
  (when (timerp nagy-mode-line--jsvar-timer)
    (cancel-timer nagy-mode-line--jsvar-timer))
  (let ((buffer (current-buffer)))
    (setq nagy-mode-line--jsvar-timer
          (run-with-timer 0.3 nil
                          (lambda ()
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (setq nagy-mode-line--jsvar-timer nil)
                                (nagy-mode-line--jsvar-refresh))))))))

(defun nagy-mode-line--jsvar-watch-point ()
  "Schedule a badge refresh after point moved."
  (unless (eq (point) nagy-mode-line--jsvar-seen-point)
    (nagy-mode-line--jsvar-schedule)))

(defun nagy-mode-line--jsvar-init ()
  "Set up the JSON badges of the current buffer."
  (nagy-mode-line--jsvar-refresh)
  (add-hook 'after-change-functions #'nagy-mode-line--jsvar-schedule nil t)
  (add-hook 'post-command-hook #'nagy-mode-line--jsvar-watch-point nil t))

(add-hook 'js-json-mode-hook #'nagy-mode-line--jsvar-init)

(defvar-local nagy-mode-line-url-knowledge
  '((url-knowledge-url ((:eval (propertize (url-knowledge-pretty-print url-knowledge-url) 'face '(:inherit (show-paren-match bold)))) " "))))

(put 'nagy-mode-line-url-knowledge 'risky-local-variable t)

(defun nagy-mode-line--list-item (glyph value)
  "Return one nagy-list badge made of GLYPH and the type of VALUE."
  (let ((selected (mode-line-window-selected-p)))
    (concat (propertize glyph 'face (if selected 'nagy-intense-cyan 'nagy-subtle-cyan))
            (propertize (symbol-name (type-of value)) 'face (if selected 'nagy-subtle-cyan 'nagy-nuanced-cyan))
            " ")))

(defvar nagy-mode-line-position
  '((:eval (when display-line-numbers-mode
             (buffer-line-count-string))))
  "Mode line construct holding the line count of the current buffer.")

(put 'nagy-mode-line-position 'risky-local-variable t)

(defvar nagy-mode-line-right
  '(
    nagy-mode-line-url-knowledge
    ;; FIXME these string creations could cause garbage
    (:eval (when (derived-mode-p 'nagy-list-mode)
             (concat (nagy-mode-line--list-item "Ŧ" nagy-list--data)
                     (nagy-mode-line--list-item "ŧ" (nagy-list--data-at-point)))))

    (nagy-mode-line--jsvar (:eval (format "%s " nagy-mode-line--jsvar)))
    (nagy-mode-line--jsvar-point (:eval (format "%s " nagy-mode-line--jsvar-point)))
    ;; File Size
    (:eval
     (unless (derived-mode-p 'exwm-mode 'magit-mode 'nagy-list-mode)
       (concat (file-size-human-readable (buffer-size)) "B ")))

    ;; Process
    (:eval
     (if-let* ((proc (get-buffer-process (current-buffer))))
         (concat (propertize "⚋" 'face 'font-lock-comment-face)
                 (propertize (format "%s" (or (process-id proc) 0))
                             'face 'nagy-nuanced-green))
       (if (derived-mode-p 'exwm-mode)
           `(:propertize ,(format "#x%X" exwm--id) face mode-line-buffer-id)
         nagy-mode-line-position)))))

(put 'nagy-mode-line-right 'risky-local-variable t)

(defvar-local nagy-mode-line--show-default-directory t)
(put 'nagy-mode-line--show-default-directory 'risky-local-variable t)


(defvar nagy-mode-line--default-directory-shorten-alist
  '(("/run/user/1000/" . "𝒓/")
    ("/tmp/ipfs" . "□")
    ("/nix/store" . "○")
    ("/ipfs" . "□")
    ("/tmp/t" . "⧖")
    ("/tmp/" . "⧖")))


(defun nagy-mode-line--default-directory-shorten (def-dir)
  (let* ((def-dir (expand-file-name def-dir))
         (hit (seq-find (lambda (el) (string-prefix-p (car el) def-dir))
                        nagy-mode-line--default-directory-shorten-alist)))
    (abbreviate-file-name
     (if hit
         (concat (cdr hit) (substring def-dir (length (car hit))))
       def-dir))))
(memoize #'nagy-mode-line--default-directory-shorten)

(defvar nagy-mode-line-default-directory-format
  '(:eval (propertize (nagy-mode-line--default-directory-shorten default-directory)
                      'face `(:inherit ,(if (mode-line-window-selected-p) '(dired-directory) nil)
                                       :weight bold
                                       :height 1.2))))

(put 'nagy-mode-line-default-directory-format 'risky-local-variable t)

(defun nagy-mode-line-init ()
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  mode-line-modified
                  (:eval (unless (derived-mode-p 'dired-mode)
                           '(:propertize "%b " face (:height 1.2 :inherit mode-line-buffer-id))))
                  (:eval (unless (or (derived-mode-p 'exwm-mode)
                                     (derived-mode-p 'dired-mode))
                           mode-line-modes)) ; already includes a space at the end
                  (nagy-mode-line--show-default-directory ("" nagy-mode-line-default-directory-format))
                  mode-line-misc-info
                  mode-line-format-right-align
                  nagy-mode-line-right))

  (setq-default mode-line-modified '((buffer-file-name "%+ "))))

(nagy-mode-line-init)

(provide 'nagy-mode-line)
;;; nagy-mode-line.el ends here
