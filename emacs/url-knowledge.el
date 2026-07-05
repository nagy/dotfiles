;;; url-knowledge.el --- url info mode -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") anaphora pcase-url nagy-emacs nagy-use-package)
;;; Commentary:

;;; Code:

(defvar url-knowledge-shorten-alist
  '(("github.com" . "𝑮𝑯")
    ("gitlab.com" . "𝑮𝑳")))

(defvar-local url-knowledge-url nil)
(put 'url-knowledge-url 'permanent-local t)

(defvar url-knowledge--known-configs nil)

(cl-defstruct (url-knowledge-config
               (:constructor url-knowledge--make-config))
  buffer)


;;;###autoload
(cl-defun url-knowledge-make (name &key
                                   buffer)

  (let ((config (url-knowledge--make-config
                 :buffer buffer)))

    (setf (alist-get name url-knowledge--known-configs nil nil #'equal) config)))

(defun url-knowledge--get-url ()
  (unless (file-remote-p default-directory)
    (with-memoization url-knowledge-url
      (cl-loop for config in url-knowledge--known-configs
               for cdr = (cdr config)
               thereis (ignore-errors
                         (funcall (url-knowledge-config-buffer cdr)))))))

(defun url-knowledge--get-url-force ()
  ;; (setq-local url-knowledge-url nil)
  (url-knowledge--get-url))

(defun url-knowledge-browse-url ()
  (interactive)
  (browse-url (url-knowledge--get-url)))

(defun url-knowledge-kill-url ()
  (interactive)
  (let ((url (url-knowledge--get-url)))
    (kill-new url)
    (message "Killed: %s" url)))

;;; Contrib

(declare-function magit-get-current-remote "magit")
(declare-function magit-get "magit")
(declare-function browse-at-remote-get-url "browse-at-remote")
(declare-function project-root "project")
;; (declare-function with-directory "nagy-emacs")

(url-knowledge-make
 "Magit"
 :buffer
 (lambda ()
   (pcase major-mode
     ((derived 'magit-revision-mode)
      (browse-at-remote-get-url))
     ((or (derived 'magit-mode)
          (derived 'dired-mode))
      (aand (project-current)
            (with-directory (project-root it)
              (alet (magit-get "--local" (format "remote.%s.url" (magit-get-current-remote)))
                (when (string-prefix-p "https://" it)
                  it))))))))

(declare-function dollar "dash-shell")
(defun pypi-browse-url (url &rest _args)
  (pcase-exhaustive url
    ((url host filename)
     (setq filename (string-replace "/project/" "" filename))
     (switch-to-buffer (generate-new-buffer (concat "*pypi*" filename)))
     (dollar (format "https://%s/pypi/%s/json"
                     host
                     (string-remove-suffix "/" filename)))
     (setq-local url-knowledge-url url))))


(defvar browse-url-handlers)
(add-to-list 'browse-url-default-handlers '("^https://pypi\\.org/project/" . pypi-browse-url))


(defvar-local url-knowledge-pretty-printed nil)
(put 'url-knowledge-pretty-printed 'permanent-local t)
(defun url-knowledge-pretty-print (url)
  (with-memoization url-knowledge-pretty-printed
    (setq url (string-remove-prefix "https://" url))
    (cl-loop for el in url-knowledge-shorten-alist
             for prefix = (car el)
             for replacement = (cdr el)
             do
             (setq url (string-replace prefix replacement url)))
    url))

(provide 'url-knowledge)
;;; url-knowledge.el ends here
