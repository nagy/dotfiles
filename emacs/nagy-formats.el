;;; nagy-formats.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: January 27, 2023
;; Modified: January 27, 2023
;; Version: 0.0.1
;; Keywords: convenience, files
;; Package-Requires: ((emacs "29.1") evil yaml-mode)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'evil)
(require 'yaml-mode)
(require 'wat-mode)
(eval-when-compile
  (require 'cl-lib))

(defun nagy-formats--call-converter (from to)
  (let ((result (nagy-formats-convert from to)))
    (cl-typecase result
      (string (shell-command-on-region (point-min) (point-max) result t 'no-mark "*format-error*"))
      (t ;; The function has already done the conversion
       )))
  (goto-char (point-min)))

(defun nagy-formats-do-convert (into-mode)
  (interactive "SDo Convert: ")
  ;; TODO region-active-p
  ;; TODO set revert-buffer function
  ;; TODO evil mode g- prefix
  ;; TODO embark integration ?
  (let ((oldbuf (current-buffer))
        (newbuf (generate-new-buffer (format "Into-%s" into-mode))))
    (copy-to-buffer newbuf (point-min) (point-max))
    (add-hook 'after-change-functions
              (lambda (&rest _args)
                (when (and (buffer-live-p newbuf)
                           (not (buffer-local-value 'buffer-read-only newbuf))
                           (not (eq evil-state 'insert)))
                  (with-current-buffer newbuf (erase-buffer))
                  (copy-to-buffer newbuf (point-min) (point-max))
                  (with-current-buffer newbuf
                    (nagy-formats--call-converter (buffer-local-value 'major-mode oldbuf) into-mode)
                    (cl-loop for f in after-change-functions
                             do
                             (when (functionp f)
                               (funcall f (point-min) (point-max) (point-max)))))))
              nil t)
    (add-hook 'evil-insert-state-exit-hook
              (lambda (&rest _args)
                (when (and (buffer-live-p newbuf)
                           (not (buffer-local-value 'buffer-read-only newbuf)))
                  (let ((inhibit-modification-hooks t))
                    (with-current-buffer newbuf (erase-buffer))
                    (copy-to-buffer newbuf (point-min) (point-max))
                    (with-current-buffer newbuf
                      (nagy-formats--call-converter (buffer-local-value 'major-mode oldbuf) into-mode)
                      (cl-loop for f in after-change-functions
                               do
                               (when (functionp f)
                                 (funcall f (point-min) (point-max) (point-max))))))))
              nil t)
    (with-current-buffer newbuf
      (nagy-formats--call-converter (buffer-local-value 'major-mode oldbuf) into-mode)
      (set-window-buffer nil (current-buffer)))))
(keymap-global-set "H-M-b" #'nagy-formats-do-convert)

(cl-defgeneric nagy-formats-convert (from to)
  (message "converting from %S to %S" from to ))

(cl-defmethod nagy-formats-convert ((from (derived-mode js-json-mode)) (to (eql yaml-mode)))
  (ignore from to )
  (shell-command-on-region (point-min) (point-max) ",json,yaml" t 'no-mark (generate-new-buffer "*Format Errors*"))
  (unless (eq major-mode 'yaml-mode)
    (yaml-mode)))

(cl-defmethod nagy-formats-convert ((_from (derived-mode js-json-mode)) (_to (eql conf-toml-mode)))
  (shell-command-on-region (point-min) (point-max) "yj -jt -i" t 'no-mark (generate-new-buffer "*Format Errors*"))
  (unless (eq major-mode 'conf-toml-mode)
    (conf-toml-mode)))

(cl-defmethod nagy-formats-convert ((_from (derived-mode conf-toml-mode)) (_to (eql js-json-mode)))
  (shell-command-on-region (point-min) (point-max) "yj -tj -i" t 'no-mark (generate-new-buffer "*Format Errors*"))
  (unless (eq major-mode 'js-json-mode)
    (js-json-mode)))

(cl-defmethod nagy-formats-convert ((_from (derived-mode conf-toml-mode)) (_to (eql yaml-mode)))
  (shell-command-on-region (point-min) (point-max) "yj -ty" t 'no-mark (generate-new-buffer "*Format Errors*"))
  (unless (eq major-mode 'yaml-mode)
    (yaml-mode)))

(cl-defmethod nagy-formats-convert (_from (_to (eql b64)))
  "base64")

(cl-defmethod nagy-formats-convert (_from (_to (eql b64d)))
  "base64 -d")

(cl-defmethod nagy-formats-convert (_from (_to (eql b32)))
  "base32")

(cl-defmethod nagy-formats-convert (_from (_to (eql hex)))
  "hexdump -vC")

(cl-defmethod nagy-formats-convert (_from (_to (eql wasm2wat)))
  (shell-command-on-region (point-min) (point-max) "wasm2wat -" t 'no-mark (generate-new-buffer "*Format Errors*"))
  (unless (eq major-mode 'wat-mode)
    (wat-mode)))

(cl-defmethod nagy-formats-convert ((_from (eql yaml-mode)) (_to (eql js-json-mode)))
  (shell-command-on-region (point-min) (point-max) "yj -yj -i" t 'no-mark (generate-new-buffer "*Format Errors*"))
  (unless (eq major-mode 'js-json-mode)
    (js-json-mode)))

(cl-defmethod nagy-formats-convert (_from (_to (eql sha1)))
  "sha1sum")

(cl-defmethod nagy-formats-convert (_from (_to (eql sha2)))
  "sha256sum")

(provide 'nagy-formats)
;;; nagy-formats.el ends here
