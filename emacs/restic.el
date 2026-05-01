;;; restic.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dash-shell)

;; NIX-EMACS-PACKAGE: nagy-emacs
(require 'nagy-emacs)

;; NIX-EMACS-PACKAGE: map-extras
(require 'map-extras)

(defvar restic-program "restic")

(cl-defstruct restic repo cache)
(cl-defstruct restic-snapshot id tree time paths)
(map-extras-define-for-struct restic-snapshot (id tree time paths))

(defun restic--as-data (restic)
  (--> (dollar-json restic-program
                    "--repo" (restic-repo restic)
                    "--no-lock"
                    "snapshots"
                    "--json")
       (restic--sort-cache it)
       ))

(defun restic--sort-cache (cache)
  (--sort (if (string= (gethash "time" it) (gethash "time" other))
              (string> (gethash "id" it) (gethash "id" other))
            (string> (gethash "time" it) (gethash "time" other)) )
          cache))

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
  (if (not (restic-cache sequence))
      (setf (restic-cache sequence) (restic--as-data sequence)))
  (length (restic-cache sequence)))
(cl-defmethod seq-elt ((sequence restic) n)
  (if (not (restic-cache sequence))
      (setf (restic-cache sequence) (restic--as-data sequence)))
  (alet (elt (restic-cache sequence) n)
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
