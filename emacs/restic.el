;;; restic.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dash-shell)

;; NIX-EMACS-PACKAGE: nagy-emacs
(require 'nagy-emacs)

;; NIX-EMACS-PACKAGE: map-extras
(require 'map-extras)

(defvar restic--known nil)

(defvar restic-program "restic")

(cl-defstruct (restic (:constructor restic--make))
  name repo runtime-env -gathered)

;;;###autoload
(cl-defun restic-make (name &key repo runtime-env)
  (declare (indent 1))
  (setf (alist-get name restic--known nil nil #'equal)
        (restic--make :name name
                      :repo repo
                      :runtime-env runtime-env
                      :-gathered nil
                      )))

(cl-defstruct restic-snapshot id tree time paths)
(map-extras-define-for-struct restic-snapshot (id tree time paths))

(defun restic-call-process (restic &rest args)
  (cl-assert (restic-p restic))
  (let* ((coding-system-for-read
          (if (member "--json" args) 'utf-8 coding-system-for-read)))
    (with-environment-variables
        (("RESTIC_REPOSITORY" (restic-repo restic)))
      (pcase-dolist (`(,name ,value) (restic-runtime-env restic))
        (setenv name value))
      (cl-assert (zerop (apply #'call-process
                               restic-program
                               nil ;; infile
                               '(t nil) ;; buffer output
                               nil      ;; display
                               args))))))

(cl-defmethod gather ((obj restic))
  (with-memoization (oref obj -gathered)
    (with-temp-buffer
      (restic-call-process obj "snapshots" "--json" "--no-lock")
      (goto-char (point-min))
      (--> (json-parse-buffer)
           (--sort (if (string= (gethash "time" it) (gethash "time" other))
                       (string< (gethash "id" it) (gethash "id" other))
                     (string< (gethash "time" it) (gethash "time" other)) )
                   it)
           (nreverse it)))))

(cl-defmethod ungather ((obj restic))
  (setf (oref obj -gathered) nil))

;; * map.el Integration

(add-to-list 'file-name-handler-alist '("\\`/restic/?" . map-file-name-handler))
(add-to-list 'map--fileprefix-alist
             `("/restic/" . ,(make-map-transformer-via-extension 'restic--known ".restic.json"))
             )

;; * GUI

;; NIX-EMACS-PACKAGE: nagy-list
(require 'nagy-list)

(cl-defmethod nagy-list-sym2 ((_restic restic))
  'restic)

(nagy-list-make 'restic
                :suffix ".restic"
                :column-names (lambda ()
                                '(id
                                  tree
                                  time
                                  ;; hostname
                                  paths
                                  ))
                :format-cell (lambda (column value)
                               (pcase column
                                 ('id (propertize (truncate-string-to-width (or value "") 8) 'font-lock-face 'magit-hash))
                                 ('tree (propertize (truncate-string-to-width (or value "") 8) 'font-lock-face 'magit-hash))
                                 ('time :date)))
                :column-width (lambda (column)
                                (pcase column
                                  ('id 8)
                                  ('tree 8)
                                  ('hostname 10)
                                  ('time 16))))

;; * seq.el Integration
(cl-defmethod seqp ((_object restic))
  t)
(cl-defmethod seq-length ((sequence restic))
  (length (restic--gathered sequence)))
(cl-defmethod seq-elt ((sequence restic) n)
  (alet (elt (restic--gathered sequence) n)
    (make-restic-snapshot :id (or (map-elt it 'id) (map-elt it "id"))
                          :tree (or (map-elt it 'tree) (map-elt it "tree"))
                          :time (or  (map-elt it 'time) (map-elt it "time"))
                          :paths (or (map-elt it 'paths) (map-elt it "paths"))
                          )))
(cl-defmethod seq-do (function (sequence restic))
  (dotimes (i (seq-length sequence))
    (funcall function (seq-elt sequence i))))

;; NIX-EMACS-PACKAGE: llama
(require 'llama)

(cl-defmethod seq-contains-p ((sequence restic) (elt string) &optional _testfn)
  ;;;  in case of a string search both snapshot id and tree id
  (seq-find (##or (string-prefix-p elt (map-elt %1 "id"))
                  (string-prefix-p elt (map-elt %1 "tree")))
            (restic--gathered sequence)))

;; * Thingatpt Integration

(defun restic-snapshot-at-point ()
  (when (and (derived-mode-p 'nagy-list-mode)
             (eq 'restic nagy-list--sym))
    (atypecase (nagy-list--data-at-point)
      (restic-snapshot it)
      (t (make-restic-snapshot :id (or (map-elt it 'id) (map-elt it "id"))
                               :tree (or (map-elt it 'tree) (map-elt it "tree"))
                               :time (or  (map-elt it 'time) (map-elt it "time"))
                               :paths (or (map-elt it 'paths) (map-elt it "paths"))
                               )))
    ))

(require 'thingatpt)

(add-to-list 'thing-at-point-provider-alist
             '(restic-snapshot . restic-snapshot-at-point))

(provide 'restic)
;;; restic.el ends here
