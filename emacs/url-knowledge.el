;;; url-knowledge.el --- url info mode -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

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
  (cl-loop for config in url-knowledge--known-configs
           for cdr = (cdr config)
           thereis (funcall (url-knowledge-config-buffer cdr))))

;;; Contrib

(declare-function elfeed-entry-link "elfeed-db")
(declare-function elfeed-search-selected "elfeed-search")
(defvar elfeed-show-entry)

(url-knowledge-make
 "Elfeed"
 :buffer
 (lambda ()
   (pcase major-mode
     ('elfeed-search-mode
      (elfeed-entry-link (car (elfeed-search-selected))))
     ('elfeed-show-mode
      (elfeed-entry-link elfeed-show-entry)))))

(declare-function eww-current-url "eww")

(url-knowledge-make
 "Eww"
 :buffer
 (lambda ()
   (pcase major-mode
     ('eww-mode
      (eww-current-url)))))

(url-knowledge-make
 "Magit"
 :buffer
 (lambda ()
   (magit-get "--local" (format "remote.%s.url" (magit-get-current-remote)))))

(provide 'url-knowledge)
;;; url-knowledge.el ends here
