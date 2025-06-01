;;; jsonrpc-noenvelope.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") jsonrpc anaphora ht)

(require 'jsonrpc)
(require 'anaphora)
(require 'ht)

(defclass jsonrpc-noenvelope (jsonrpc-process-connection)
  ())

(defun jsonrpc-noenvelope--filter (proc msg)
  (with-current-buffer (process-buffer proc)
    (internal-default-process-filter proc msg)
    ;; TODO FIXME only parsing one json in each filter invocation can
    ;; result in bugs if multiple json objects are printed very
    ;; quickly by the process.
    (let* ((parsed (json-parse-string msg))
           (conn (process-get proc 'jsonrpc-connection)))
      ;; Now, time to notify user code of one or more messages in
      ;; order.  Very often `jsonrpc-connection-receive' will exit
      ;; non-locally (typically the reply to a request), so do
      ;; this all this processing in top-level loops timer.
      (let ((time (current-time))
            (timer (timer-create)))
        (timer-set-time timer time)
        (timer-set-function timer
                            (lambda (conn _msg parsed)
                              (let ((sending (list :jsonrpc "2.0" :id (gethash "id" parsed))))
                                (awhen (gethash "error" parsed)
                                  (setq sending (plist-put sending :error (list :code (gethash "code" it) :message (gethash "message" it)))))
                                (awhen (gethash "result" parsed)
                                  (setq sending (plist-put sending :result it)))
                                (jsonrpc-connection-receive conn sending)))
                            (list conn msg parsed))
        (timer-activate timer)))))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-noenvelope)
                                       &rest args
                                       &key
                                       id
                                       method
                                       params
                                       (_result nil result-supplied-p)
                                       _error
                                       _partial)
  (process-send-string (jsonrpc--process connection)
                       (concat (json-serialize (ht ("jsonrpc" "2.0")
                                                   ("id" id)
                                                   ("method" (string-remove-prefix ":" (symbol-name method)))
                                                   ("params" params)))
                               "\n")))

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
