;;; restic.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dash-shell)

;; NIX-EMACS-PACKAGE: nagy-emacs
(require 'nagy-emacs)

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

(defun get-restic (name)
  (map-elt restic--known name))

(cl-defstruct restic-snapshot
  restic id treeid -data)

(defun restic-call-process-from (name from infile &rest args)
  (let* ((config (get-restic name))
         (from-config (get-restic from))
         (coding-system-for-read (if (member "--json" args) 'utf-8 coding-system-for-read))
         )
    (with-environment-variables
        (("RESTIC_REPOSITORY" (restic-repo config))
         ("RESTIC_FROM_REPOSITORY" (if from (restic-repo from-config) ""))
         ("RESTIC_CACHE_DIR" (concat temporary-file-directory "/restic-cache"))
         ("RESTIC_READ_CONCURRENCY" "1")
         ;; ("RESTIC_CACHE_DIR" (format "%s/restic-from-cache" temporary-file-directory))
         )
      (pcase-dolist (`(,name ,value)
                     (append (restic-runtime-env config)
                             (and from-config (restic-runtime-env from-config))))
        (setenv name value))
      ;; To prevent non-existing directories from breaking this.
      (with-directory temporary-file-directory
        (cl-assert (zerop (apply #'call-process
                                 restic-program
                                 infile ;; infile
                                 t      ;; buffer output
                                 nil    ;; display
                                 args)))))))

(defun restic-call-process (name infile &rest args)
  (apply #'restic-call-process-from name nil infile args))

;; * GUI

;; NIX-EMACS-PACKAGE: nagy-list
(require 'nagy-list)

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

(cl-defmethod seq-do (function (sequence restic))
  (mapcar function (restic--gathered sequence)))

(cl-defmethod seq-length ((sequence restic))
  (length (restic--gathered sequence)))

(cl-defmethod seq-elt ((sequence restic) n)
  (elt (restic--gathered sequence) n))

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
    (pcase-let (((map id tree) (nagy-list--data-at-point)))
      (make-restic-snapshot
       :id id
       :treeid tree))))

(require 'thingatpt)

(add-to-list 'thing-at-point-provider-alist
             '(restic-snapshot . restic-snapshot-at-point))

(provide 'restic)
;;; restic.el ends here
