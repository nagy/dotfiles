;;; restic.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;; NIX-EMACS-PACKAGE: dash-shell
;; (require 'dash-shell)

;; NIX-EMACS-PACKAGE: nagy-list
(require 'nagy-list)

;; NIX-EMACS-PACKAGE: nagy-emacs
(require 'nagy-emacs)

(defvar restic--known-configs nil)

(defvar restic-program "restic")

(cl-defstruct (restic-config (:constructor restic--make-config))
  name repo command runtime-env)

;;;###autoload
(cl-defun restic-make (name &key repo command runtime-env)
  (declare (indent 1))
  (setf (alist-get name restic--known-configs nil nil #'equal)
        (restic--make-config :name name
                             :repo repo
                             :command command
                             :runtime-env runtime-env
                             )))


(provide 'restic)
;;; restic.el ends here
