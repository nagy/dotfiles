;;; yggdrasil.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "30.1") dash-shell nagy-emacs nagy-list)

(require 'dash-shell)
(require 'nagy-list)

(require 'nagy-emacs)

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
