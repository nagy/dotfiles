;;; restic.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dash-shell)

;; NIX-EMACS-PACKAGE: nagy-emacs
(require 'nagy-emacs)

;; NIX-EMACS-PACKAGE: map-extras
(require 'map-extras)

;; NIX-EMACS-PACKAGE: memoize
(require 'memoize)

(defvar restic--known nil)

(defvar restic-program "restic")

(cl-defstruct (restic (:constructor restic--make))
  repo)

;;;###autoload
(cl-defun restic-make (name &key repo)
  (declare (indent 1))
  (setf (alist-get name restic--known nil nil #'equal)
        (restic--make :repo repo
                      )))

(cl-defstruct restic-snapshot id tree time paths)
(map-extras-define-for-struct restic-snapshot (id tree time paths))

(defun restic-call-process (restic &rest args)
  (cl-assert (restic-p restic))
  (let* ((coding-system-for-read
          (if (member "--json" args) 'utf-8 coding-system-for-read)))
    (with-environment-variables
        (("RESTIC_REPOSITORY" (restic-repo restic)))
      (cl-assert (zerop (apply #'call-process
                               restic-program
                               nil ;; infile
                               '(t nil) ;; buffer output
                               nil      ;; display
                               args))))))

(defun restic--as-data (restic)
  (with-temp-buffer
    (restic-call-process restic "--no-lock" "snapshots" "--json")
    (goto-char (point-min))
    (--> (json-parse-buffer)
         (--sort (if (string= (gethash "time" it) (gethash "time" other))
                     (string< (gethash "id" it) (gethash "id" other))
                   (string< (gethash "time" it) (gethash "time" other)) )
                 it)
         (nreverse it))))
(memoize #'restic--as-data)

;; * map.el Integration

;; (add-to-list 'file-name-handler-alist '("\\`/restic/?" . map-file-name-handler))
;; (add-to-list 'map--fileprefix-alist
;;              `("/restic/" . ,(make-map-transformer-via-extension 'restic--known ".restic.json"))
;;              )

;; * GUI

;; NIX-EMACS-PACKAGE: nagy-list
(require 'nagy-list)

(defun nagy-restic-list-view ()
  (setq-local nagy-list--columns
              `((id 8 ,(lambda (value)
                         (propertize
                          (truncate-string-to-width (or value "") 8)
                          'font-lock-face 'magit-hash)))
                (tree 8 ,(lambda (value)
                           (propertize
                            (truncate-string-to-width (or value "") 8)
                            'font-lock-face 'magit-hash)))
                (time 16 :date)
                (paths nil)))
  (nagy-list-mode))
(add-to-list 'auto-mode-alist '("\\.restic\\.json\\'" . nagy-restic-list-view))

;; * seq.el Integration
(cl-defmethod seqp ((_object restic))
  t)
(cl-defmethod seq-length ((sequence restic))
  (length (restic--as-data sequence)))
(cl-defmethod seq-elt ((sequence restic) n)
  (alet (elt (restic--as-data sequence) n)
    (make-restic-snapshot :id (map-elt it "id")
                          :tree (map-elt it "tree")
                          :time (map-elt it "time")
                          :paths (map-elt it "paths")
                          )))
(cl-defmethod seq-do (function (sequence restic))
  (dotimes (i (seq-length sequence))
    (funcall function (seq-elt sequence i))))

(provide 'restic)
;;; restic.el ends here
