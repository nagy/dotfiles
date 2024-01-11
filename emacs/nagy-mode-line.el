;;; nagy-mode-line.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-

(defun nagy-mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (propertize " "
              'display `(space :align-to (- (+ right right-fringe right-margin) ,reserve))
              'face face))

(defvar nagy-mode-line-right
  '((:eval
     (if (or (get-buffer-process (current-buffer))
             (string= "*elfeed-vultr*" (buffer-name)))
         (if (get-buffer-process (current-buffer))
             (format "%s%d" (propertize "⚋" 'face 'font-lock-comment-face) (process-id (get-buffer-process (current-buffer)))))
       (if (derived-mode-p 'exwm-mode)
           `(:propertize ,(number-to-string exwm--id) face mode-line-buffer-id)
         mode-line-position)))))
(put 'nagy-mode-line-right 'risky-local-variable t)

(defun nagy-mode-line-init ()
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  ;; mode-line-front-space
                  ;; mode-line-mule-info
                  ;; mode-line-client
                  mode-line-modified
                  ;; mode-line-remote
                  ;; mode-line-frame-identification
                  ;; cannot use this because of dired
                  ;; mode-line-buffer-identification
                  (:propertize "%b " face mode-line-buffer-id)
                  ;; mode-line-position ""
                  ;; (vc-mode vc-mode)
                  (:eval (unless (derived-mode-p 'exwm-mode)
                           mode-line-modes))       ; already includes a space at the end
                  (:eval (unless (derived-mode-p 'exwm-mode))
                         (:propertize (:eval
                                       (propertize (abbreviate-file-name default-directory)
                                                   'face (if (buffer-file-name)
                                                             '(:inherit (bold dired-directory))
                                                           '(:inherit dired-directory))))))
                  mode-line-misc-info
                  ;; mode-line-end-spaces
                  (:eval (nagy-mode-line-fill (if (mode-line-window-selected-p)
                                                  'mode-line
                                                'mode-line-inactive)
                                              (length (format-mode-line nagy-mode-line-right))))
                  nagy-mode-line-right))
  (setq mode-line-modified '((buffer-file-name "%+ ")))
  (setq-default mode-line-modified mode-line-modified)
  (setq mode-line-position '("L%l/" (:eval (buffer-line-count-string)) ""))
  ;; (setq mode-line-buffer-identification (list (propertize "%b" 'face 'mode-line-buffer-id)))
  ;; (setq-default mode-line-buffer-identification (list (propertize "%b" 'face 'mode-line-buffer-id)))
  ;; (setq mode-line-position '(" L%l/" (:eval (number-to-string (buffer-chars-modified-tick)))))
  ;; cannot set this because dired overrides it
  ;; (setq-default mode-line-buffer-identification '((:propertize "%b" face bold)))
  )

(provide 'nagy-mode-line)
