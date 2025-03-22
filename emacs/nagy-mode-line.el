;;; nagy-mode-line.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1") dash anaphora)

(require 'dash)
;; (require 'anaphora)

(defvar-local nagy-mode-line--jsvar nil)
(defvar-local nagy-mode-line--jsvar-point nil)


(defun nagy-mode-line--jsvar-calc ()
  (when (and (derived-mode-p 'js-json-mode)
             (< (buffer-size) large-file-warning-threshold))
    (alet (ignore-errors (save-excursion (json-parse-buffer)))
      (when it
        (pcase-exhaustive (type-of it)
          ('string (format "S%d" (length it)))
          ('hash-table (format "H%d" (hash-table-count it)))
          ('integer (format "N%d" it))
          ('float (format "F%f" it))
          ('symbol (format "%S" it))
          ('vector (format "A%d" (length it))))))
    ))

(defun nagy-mode-line--jsvar-update ()
  (setq nagy-mode-line--jsvar-point (aand (nagy-mode-line--jsvar-calc) (propertize it 'face 'button)))
  (unless nagy-mode-line--jsvar
    (setq nagy-mode-line--jsvar (or nagy-mode-line--jsvar ; this should rather be memoized on the buffer content
                                    (save-excursion (goto-char (point-min))
                                                    (aand
                                                     (nagy-mode-line--jsvar-calc)
                                                     (propertize it 'face 'nerd-icons-lsilver))))))
  (force-mode-line-update)
  )

(run-with-idle-timer 0.1 t #'nagy-mode-line--jsvar-update)

(defun nagy-mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (propertize " "
              'display `(space :align-to (- (+ right right-fringe right-margin) ,reserve))
              'face face))

;; (memoize 'nagy-mode-line-fill)
;; (memoize-restore 'nagy-mode-line-fill)

(defvar nagy-mode-line-right
  ;; FIXME these string creations could cause garbage
  '(
    (nagy-mode-line--jsvar (:eval (concat (format "%s" nagy-mode-line--jsvar) " ")))
    (nagy-mode-line--jsvar-point (:eval (concat (format "%s" nagy-mode-line--jsvar-point) " ")))
    (:eval
     (unless (derived-mode-p 'exwm-mode)
       (propertize
        (concat (file-size-human-readable (buffer-size)) "B ")
        ;; 'face (if (mode-line-window-selected-p)
        ;;           'line-number
        ;;         'mode-line-inactive)
        )))
    (:eval
     (if (get-buffer-process (current-buffer))
         (if (get-buffer-process (current-buffer))
             (format "%s%d" (propertize "⚋" 'face 'font-lock-comment-face)
                     (or
                      (process-id (get-buffer-process (current-buffer)))
                      0)))
       (if (derived-mode-p 'exwm-mode)
           `(:propertize ,(format "#x%X" exwm--id) face mode-line-buffer-id)
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
                  (:propertize "%b " face (:height 1.2 :inherit mode-line-buffer-id))
                  ;; mode-line-position ""
                  ;; (vc-mode vc-mode)
                  (:eval (unless (derived-mode-p 'exwm-mode)
                           mode-line-modes))       ; already includes a space at the end
                  (:eval (unless (derived-mode-p 'exwm-mode)
                           '(:eval (propertize (abbreviate-file-name
                                                (--> default-directory
                                                     (string-replace "%" "%%" it)
                                                     (string-replace "/tmp/t" "⧖" it)
                                                     (string-replace "/nix/store" "○" it)
                                                     ))
                                               'face '(:inherit (dired-directory)
                                                                :weight bold
                                                                :height 1.2)
                                               ))))
                  mode-line-misc-info
                  ;; (:eval (format " BFF:%S" buffer-file-format))
                  ;; container-list-mode-line-format
                  ;; (:eval (get-container-line-format))
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

(nagy-mode-line-init)
;; (add-hook 'before-init-hook #'nagy-mode-line-init)

(provide 'nagy-mode-line)
