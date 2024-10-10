;;; url-knowledge.el --- url info mode -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-

;; Package-Requires: ((emacs "29.1") nagy-use-package)
;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'nagy-use-package)

(defvar-local url-knowledge-url nil)
(put 'url-knowledge-url 'permanent-local t)

(defvar url-knowledge--known-configs nil)

(cl-defstruct (url-knowledge-config
               (:constructor url-knowledge--make-config))
  buffer
  clean
  smudge
  collapse
  )

(cl-defun url-knowledge-make (name &key
                                   buffer
                                   clean
                                   smudge
                                   collapse)
  (let ((config (url-knowledge--make-config
                 :buffer buffer
                 :clean clean
                 :smudge smudge
                 :collapse collapse)))
    (prog1 config
      (setf (alist-get name url-knowledge--known-configs nil nil #'equal) config))))

(defun url-knowledge--get-url ()
  (or url-knowledge-url
      (setq url-knowledge-url
            (cl-loop for config in url-knowledge--known-configs
                     for cdr = (cdr config)
                     thereis (ignore-errors
                               (funcall (url-knowledge-config-buffer cdr)))))))

;; (dolist (hook '(magit-mode-hook elfeed-show-mode-hook))
;;   (add-hook hook #'url-knowledge--get-url))

;;; Contrib

(declare-function elfeed-entry-link "elfeed-db")
(declare-function elfeed-search-selected "elfeed-search")
(defvar elfeed-show-entry)

(url-knowledge-make
 "Elfeed"
 :buffer
 (lambda ()
   (pcase major-mode
     ;; ((derived 'elfeed-search-mode)
     ;;  (elfeed-entry-link (car (elfeed-search-selected))))
     ((derived 'elfeed-show-mode)
      (elfeed-entry-link elfeed-show-entry)))))

(declare-function eww-current-url "eww")

(url-knowledge-make
 "Eww"
 :buffer
 (lambda ()
   (pcase major-mode
     ((derived 'eww-mode)
      (eww-current-url)))))

(declare-function magit-get-current-remote "magit")
(declare-function magit-get "magit")
(declare-function browse-at-remote-get-url "browse-at-remote")

(url-knowledge-make
 "Magit"
 :buffer
 (lambda ()
   (pcase major-mode
     ((derived 'magit-revision-mode)
      (browse-at-remote-get-url))
     ((derived 'magit-mode)
      (magit-get "--local" (format "remote.%s.url" (magit-get-current-remote)))))))

(provide 'url-knowledge)
;;; url-knowledge.el ends here
