;;; jsonrpc-noenvelope.el --- JSON-RPC without HTTP envelope  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") (jsonrpc "1.0"))
;; Keywords: comm, processes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library provides `jsonrpc-noenvelope', a subclass of
;; `jsonrpc-process-connection' that communicates with processes
;; speaking raw JSON-RPC 2.0 over stdio as newline-delimited JSON
;; objects, *without* the HTTP-style Content-Length envelope.
;;
;; Use this when your subprocess reads one JSON object per line
;; from stdin and writes one JSON object per line to stdout,
;; instead of the Content-Length header format that the standard
;; `jsonrpc-process-connection' expects.
;;
;;; Code:

(require 'jsonrpc)
(require 'cl-lib)

;;;###autoload
(defclass jsonrpc-noenvelope (jsonrpc-process-connection)
  ()
  :documentation
  "JSONRPC connection over stdio using newline-delimited JSON.
Unlike `jsonrpc-process-connection', this class does not use
Content-Length headers.  Each JSON-RPC message is sent as a
single line of JSON, and received messages are parsed by reading
complete JSON objects from the process buffer.")

(defun jsonrpc-noenvelope--iter ()
  "Find and parse the next complete JSON object at point.
Move point past the parsed object and return it as a plist.
Return nil if no complete JSON object is found."
  (let ((p (point))
        ret)
    (save-excursion
      (when-let* ((res (ignore-errors (json-parse-buffer
                                       :object-type 'plist
                                       :null-object nil
                                       :false-object :json-false))))

        (setq p (point))
        (setq ret res)))
    (goto-char p)
    ret))

(defun jsonrpc-noenvelope--filter (proc msg)
  "Process filter for `jsonrpc-noenvelope' connections.
Inserts MSG into PROC's buffer, then parses all complete
JSON objects and dispatches them via `jsonrpc-connection-receive'."
  (with-current-buffer (process-buffer proc)
    (let ((point-before (point)))
      ;; Insert raw data at the end of the buffer, then return
      ;; point to where it was (save-excursion doesn't always work
      ;; across process filters, so we track it manually).
      (save-excursion
        (goto-char (point-max))
        (internal-default-process-filter proc msg))
      (goto-char point-before)
      ;; Parse all complete JSON objects from the buffer
      (while-let ((parsed (jsonrpc-noenvelope--iter)))
        (let* ((conn (process-get proc 'jsonrpc-connection))
               (time (current-time))
               (timer (timer-create)))
          (timer-set-time timer time)
          (timer-set-function
           timer
           (lambda (conn msg)
             (jsonrpc-connection-receive conn msg))
           (list conn
                 ;; Build the plist in the format that
                 ;; `jsonrpc-connection-receive' expects.  Keep
                 ;; method as a string -- receive interning it.
                 (list :jsonrpc "2.0"
                       :id (plist-get parsed :id)
                       :method (plist-get parsed :method)
                       :params (plist-get parsed :params)
                       :result (plist-get parsed :result)
                       :error (when-let* ((e (plist-get parsed :error)))
                                (list :code (plist-get e :code)
                                      :message (plist-get e :message)
                                      :data (plist-get e :data))))))
          (timer-activate timer))))))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-noenvelope)
                                       &rest args
                                       &key
                                       id
                                       method
                                       _params
                                       (_result nil result-supplied-p)
                                       error
                                       _partial)
  "Send a JSONRPC message to CONNECTION as a newline-delimited JSON line."
  (when method
    ;; Sanitize method into a string (mirrors what
    ;; `jsonrpc-process-connection' does for envelope mode)
    (setq args
          (plist-put args :method
                     (cond ((keywordp method) (substring (symbol-name method) 1))
                           ((symbolp method) (symbol-name method))
                           ((stringp method) method)
                           (t (error "[jsonrpc] invalid method %s" method))))))
  (let* ((converted (jsonrpc-convert-to-endpoint connection args
                                                 (cond ((or result-supplied-p error) 'reply)
                                                       (id 'request)
                                                       (method 'notification))))
         (json (jsonrpc--json-encode converted)))
    (process-send-string (jsonrpc--process connection)
                         (concat json "\n"))))

(cl-defmethod initialize-instance :after ((conn jsonrpc-noenvelope) _slots)
  "Set up the no-envelope process filter.
This runs after the parent `jsonrpc-process-connection' has
already set up the process, stderr buffer, sentinel, etc."
  (let ((proc (jsonrpc--process conn)))
    (set-process-filter proc #'jsonrpc-noenvelope--filter)
    (set-process-query-on-exit-flag proc nil)
    (setf (process-get proc 'jsonrpc-connection) conn)
    ;; The parent already created the process buffer; just ensure
    ;; it's ready for our filter
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (buffer-disable-undo)
        (setq buffer-read-only t)
        (erase-buffer)))))

(provide 'jsonrpc-noenvelope)
;;; jsonrpc-noenvelope.el ends here
