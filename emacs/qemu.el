;;; qemu.el --- -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;; NIX-EMACS-PACKAGE: dash-shell
(require 'dash-shell)
;; NIX-EMACS-PACKAGE: nix-mode
(require 'nix)

;; (defvar qemu--known nil)

(cl-defstruct (qemu (:constructor qemu--make-config))
  cmd
  -process)

(cl-defun make-qemu-process (&key name cdrom)
  (let ((args `("qemu-system-x86_64"
                "-S"
                ,@(when cdrom (list "-boot" "d" "-cdrom" cdrom))
                "-enable-kvm"
                "-m" "8192"
                "-smp" "8"
                "-qmp" "unix:/tmp/baz.sock,server,wait=off"
                )))
    `(make-process :name ,name
                   :buffer "*qemu*"
                   :command ,args))
  )

;; (defvar *qemu* (make-qemu-process
;;                 :name "qemu-alpine"
;;                 :cdrom "alpine-standard-3.21.3-x86_64.iso"))

;; ;;;###autoload
;; (cl-defun qemu-make (name &key cmd)
;;   (declare (indent 1))
;;   (setf (alist-get name qemu--known nil nil #'equal)
;;         (qemu--make-config :name name
;;                             :board board
;;                             )))


;; (make-network-process :name "qmp"
;;                       :service "/tmp/baz.sock"
;;                       :family 'local
;;                       :buffer "*qmp*"
;;                       :filter (lambda (proc text)
;;                                 (with-current-buffer (process-buffer proc)
;;                                   (internal-default-process-filter proc text)
;;                                   (message "%S" (json-parse-string text))))
;;                       )

;; (with-current-buffer "*qmp*"
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (process-send-string proc "{ \"execute\": \"qmp_capabilities\" }\r\n")))

;; (with-current-buffer "*qmp*"
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (process-send-string proc "{ \"execute\": \"query-status\" }\r\n")))

;; (with-current-buffer "*qmp*"
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (process-send-string proc "{ \"execute\": \"qom_list\" }\r\n")))

;; (with-current-buffer "*qmp*"
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (process-send-string proc "{ \"execute\": \"human-monitor-command\", \"arguments\": {\"command-line\": \"info qtree\"} }\r\n")))

;; (with-current-buffer "*qmp*"
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (process-send-string proc "{ \"execute\": \"qom-list\", \"arguments\": {\"path\": \"/machine\"} }\r\n")))

;; (with-current-buffer "*qmp*"
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (process-send-string proc "{ \"execute\": \"query-machines\" }\r\n")))

;; (with-current-buffer "*qmp*"
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (process-send-string proc "{ \"execute\": \"cont\" }\r\n")))

;; (with-current-buffer "*qmp*"
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (process-send-string proc "{ \"execute\": \"stop\" }\r\n")))

(provide 'qemu)
;;; qemu.el ends here
