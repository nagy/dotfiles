;;; yggdrasil.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dash-shell nagy-emacs)

;; NIX-EMACS-PACKAGE: nagy-list
(require 'nagy-list)
;; NIX-EMACS-PACKAGE: map-extras
(require 'map-extras)

(defvar yggdrasil--known nil)

(defvar yggdrasil-program "yggdrasil")
(defvar yggdrasil-ctl-program "yggdrasilctl")

(cl-defstruct (yggdrasil (:constructor yggdrasil--make))
  name config endpoint command -gathered)

;;;###autoload
(cl-defun yggdrasil-make (name &key config endpoint command)
  (declare (indent 1))
  (setf (alist-get name yggdrasil--known nil nil #'equal)
        (yggdrasil--make :name name
                         :config config
                         :endpoint endpoint
                         :command command
                         :-gathered nil
                         )))

(cl-defstruct yggdrasil-peer address remote up inbound uptime key)
(map-extras-define-for-struct yggdrasil-peer (address remote up inbound uptime key))

(defvar *ygg*
  (yggdrasil-make "default"
    :endpoint "unix:///var/run/yggdrasil/yggdrasil.sock"))

(cl-defmethod yggdrasil-ctl-call-process ((yggdrasil yggdrasil) &rest args)
  (apply #'dollar-json2
         yggdrasil-ctl-program
         :endpoint (yggdrasil-endpoint yggdrasil)
         :json
         args))

(cl-defmethod gather ((obj yggdrasil))
  (with-memoization (oref obj -gathered)
    (gethash "peers"
             (yggdrasil-ctl-call-process obj "getpeers"))))

(cl-defmethod ungather ((obj yggdrasil))
  (setf (oref obj -gathered) nil))

(cl-defmethod nagy-list-sym2 ((_yggdrasil yggdrasil))
  'yggdrasil)

(nagy-list-make
 'yggdrasil
 :suffix ".yggdrasil"
 :column-names (lambda ()
                 '(address remote up inbound uptime key))
 :format-cell (lambda (column value)
                (pcase column
                  ('address :identifier)
                  ('uptime
                   (propertize
                    (format-time-string "%-Hh%-Mm%-Ss" (seconds-to-time value) 0)
                    'font-lock-face '(:inherit nagy-fg-cyan-intense)))
                  ))
 :column-width (lambda (column)
                 (pcase column
                   ('up 5)
                   ('inbound 7)
                   ('remote 30)
                   ('uptime 9)
                   ('address 38))))

;; * seq.el Integration
(cl-defmethod seqp ((_object yggdrasil))
  t)
(cl-defmethod seq-length ((sequence yggdrasil))
  (length (yggdrasil--gathered sequence)))
(cl-defmethod seq-elt ((sequence yggdrasil) n)
  (alet (elt (yggdrasil--gathered sequence) n)
    (make-yggdrasil-peer :address (or (map-elt it 'address) (map-elt it "address"))
                         :remote (or (map-elt it 'remote) (map-elt it "remote"))
                         :up (or (map-elt it 'up) (map-elt it "up"))
                         :inbound (or (map-elt it 'inbound) (map-elt it "inbound"))
                         :uptime (or (map-elt it 'uptime) (map-elt it "uptime"))
                         :key (or (map-elt it 'key) (map-elt it "key"))
                         )))
(cl-defmethod seq-do (function (sequence yggdrasil))
  (dotimes (i (seq-length sequence))
    (funcall function (seq-elt sequence i))))

(add-to-list 'file-name-handler-alist '("\\`/yggdrasil/?" . map-file-name-handler))
(add-to-list 'map--fileprefix-alist
             `("/yggdrasil/" . ,(make-map-transformer-via-extension 'yggdrasil--known ".yggdrasil.json"))
             )

(provide 'yggdrasil)
;;; yggdrasil.el ends here
