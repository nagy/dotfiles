;;; yggdrasil.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;; NIX-EMACS-PACKAGE: dash-shell
;; (require 'dash-shell)
;; NIX-EMACS-PACKAGE: nagy-list
(require 'nagy-list)
;; NIX-EMACS-PACKAGE: nagy-emacs
(require 'nagy-emacs)
;; NIX-EMACS-PACKAGE: map-extras
(require 'map-extras)

(defvar yggdrasil--known-configs nil)

(defvar yggdrasil-program "yggdrasil")
(defvar yggdrasil-ctl-program "yggdrasilctl")

(cl-defstruct (yggdrasil-config (:constructor yggdrasil--make-config))
  name config endpoint command)

;;;###autoload
(cl-defun yggdrasil-make (name &key config endpoint command)
  (declare (indent 1))
  (setf (map-elt yggdrasil--known-configs name)
        (yggdrasil--make-config :name name
                                :config config
                                :endpoint endpoint
                                :command command
                                )))


(provide 'yggdrasil)
;;; yggdrasil.el ends here
