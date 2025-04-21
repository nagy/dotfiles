;;; qemu.el --- -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;; (defvar qemu--known nil)

(cl-defstruct (qemu (:constructor qemu--make-config))
  cmd
  -process)

(provide 'qemu)
;;; qemu.el ends here
