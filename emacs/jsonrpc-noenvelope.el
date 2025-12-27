;;; jsonrpc-noenvelope.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") jsonrpc)

(require 'jsonrpc)
;; NIX-EMACS-PACKAGE: anaphora
(require 'anaphora)
;; NIX-EMACS-PACKAGE: ht
(require 'ht)

(defclass jsonrpc-noenvelope (jsonrpc-process-connection)
  ())

(defun jsonrpc-noenvelope--iter ()
  (let ((p (point))
        ret)
    (save-excursion
      (when-let ((res (ignore-errors (json-parse-buffer))))
        (setq p (point))
        (setq ret res)))
    (goto-char p)
    ret
    ))

(defun jsonrpc-noenvelope--filter (proc msg)
  (with-current-buffer (process-buffer proc)
    (let ((point-before (point)))
      (save-excursion
        (goto-char (point-max))
        (internal-default-process-filter proc msg))
      (goto-char point-before)) ;; for whatever reason, save-excursion does not work here
    (while-let ((parsed (jsonrpc-noenvelope--iter)))
      (let ((conn (process-get proc 'jsonrpc-connection)))
        ;; Now, time to notify user code of one or more messages in
        ;; order.  Very often `jsonrpc-connection-receive' will exit
        ;; non-locally (typically the reply to a request), so do
        ;; this all this processing in top-level loops timer.
        (let ((time (current-time))
              (timer (timer-create)))
          (timer-set-time timer time)
          (timer-set-function timer
                              (lambda (conn parsed)
                                (jsonrpc-connection-receive
                                 conn
                                 (append (list :jsonrpc "2.0")
                                         (list :id (gethash "id" parsed))
                                         (awhen (gethash "error" parsed)
                                           (list :error (list :code (gethash "code" it) :message (gethash "message" it))))
                                         (awhen (gethash "result" parsed) (list :result it))
                                         (awhen (gethash "method" parsed) (list :method it))
                                         (awhen (gethash "params" parsed) (list :params it))
                                         )))
                              (list conn parsed))
          (timer-activate timer))))))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-noenvelope)
                                       &rest args
                                       &key
                                       id
                                       method
                                       params
                                       (_result nil result-supplied-p)
                                       _error
                                       _partial)
  (let ((table (make-hash-table :test #'equal)))
    (puthash "jsonrpc" "2.0" table)
    (when id
      (puthash "id" id table))
    (when method
      (puthash "method" (string-remove-prefix ":" (symbol-name method)) table))
    (when params
      (puthash "params" params table))
    (process-send-string (jsonrpc--process connection)
                         (concat (json-serialize table ) "\n"))))

(cl-defmethod initialize-instance :after ((conn jsonrpc-noenvelope) slots)
  (cl-destructuring-bind (&key ((:process proc)) name &allow-other-keys) slots
    (set-process-buffer proc (get-buffer-create (format " *%s output*" name)))
    (set-process-filter proc #'jsonrpc-noenvelope--filter)
    (set-process-query-on-exit-flag proc nil)
    (setf (process-get proc 'jsonrpc-connection) conn)
    (with-current-buffer (process-buffer proc)
      (buffer-disable-undo)
      (setq buffer-read-only t))
    ))

(provide 'jsonrpc-noenvelope)
;;; jsonrpc-noenvelope.el ends here
