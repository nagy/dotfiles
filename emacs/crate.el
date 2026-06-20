;;; crate.el --- Rust crates -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "30.1") dash-shell anaphora org s nagy-emacs)

(require 'bookmark)
(require 'anaphora)
(require 'nagy-emacs)
(require 'org)
(require 's)
(require 'dash-shell)

(defvar-local crate-name nil)
(put 'crate-name 'permanent-local t)
(defvar-local crate-data nil)
(put 'crate-data 'permanent-local t)

(defgroup crate nil
  "Browse Rust crates locally."
  :group 'tools)

(defcustom crate-data-path nil
  "Path to a static.crates.io JSON dump file."
  :type 'file
  :group 'crate)

(defcustom crate-modules-program "cargo-modules"
  "Name or path of the cargo-modules executable."
  :type 'string
  :group 'crate)

(defvar crate--data-cache (make-hash-table :test #'equal))

(defun crate-list-json ()
  (with-memoization (gethash 'data crate--data-cache)
    (when (and crate-data-path (file-exists-p crate-data-path))
      (json-parse-file crate-data-path))))

(defvar crate-structure--cache (make-hash-table :test #'equal))

(defun crate-structure (name)
  (with-memoization (gethash name crate-structure--cache)
    (with-temp-buffer
      (let ((exitcode (call-process crate-modules-program nil t nil "structure" "--package" (string-replace "_" "-" name) "--lib")))
        ;; (message "ddir %s exitcode %d crate-name: %s" default-directory exitcode name)
        ;; the case in esp-hal crate
        (unless (eq 0 exitcode)
          (erase-buffer)
          (with-directory (string-replace "_" "-" name)
            (call-process crate-modules-program nil t nil "structure" "--package" (string-replace "_" "-" name) "--lib"))))
      ;; (call-process crate-modules-program nil t nil "structure" "--package" name "--lib")
      (string-remove-prefix "\n" (buffer-string)))))

(defun insert-crate-structure ()
  (let ((p (point)))
    (insert (crate-structure crate-name))
    (ansi-color-apply-on-region p (point))
    ))

(defun crate--description ()
  (awhen (gethash "description" crate-data)
    (setq it (string-replace "\n" "" it))
    (setq it (s-truncate fill-column it))
    (if (eq it :null) "" it)))

(defun crate--insert-field (label key &optional face &rest extra-props)
  "Insert LABEL (bold), then the value of KEY from `crate-data'.
If the value is nil or :null, nothing is inserted after the label.
If FACE is non-nil, propertize the value with it.
EXTRA-PROPS are additional text properties applied to the value."
  (insert (propertize label 'face 'bold))
  (let ((val (gethash key crate-data)))
    (unless (or (null val) (eq val :null))
      (insert (if face
                  (apply #'propertize val 'face face extra-props)
                val))))
  (insert "\n"))

;;;###autoload
(define-derived-mode crate-mode text-mode "Crate"
  ;; (setq-local list-buffers-directory crate-name)
  (cd temporary-file-directory)
  (setq-local bookmark-make-record-function #'crate--bookmark-make-record-function)
  (setq-local revert-buffer-function #'ignore)
  (insert (propertize "Name:          " 'face 'bold))
  (insert (propertize crate-name 'face 'org-document-title) "\n")
  (setq-local url-knowledge-url (format "https://crates.io/crates/%s" crate-name))
  (setq-local list-buffers-directory (gethash "description" crate-data))
  (insert (propertize "Description:   " 'face 'bold))
  (insert (crate--description))
  (insert "\n")
  ;; Repository (special: may cd into local checkout)
  (insert (propertize "Repository:    " 'face 'bold))
  (awhen (gethash "repository" crate-data)
    (unless (eq it :null)
      (insert (propertize it 'face 'shr-link 'mouse-face 'highlight))
      (let ((filename (format "/mnt/archive/%s.git.sqfs/" (--> it
                                                      (string-remove-prefix "https://" it)
                                                      (string-replace "/" "__" it)))))
        (when (file-exists-p filename)
          (cd filename)))))
  (insert "\n")
  (crate--insert-field "Homepage:      " "homepage" 'shr-link 'mouse-face 'highlight)
  (crate--insert-field "Documentation: " "documentation" 'shr-link 'mouse-face 'highlight)
  (crate--insert-field "Updated:       " "updated_at" 'marginalia-date)
  (insert (propertize "Id:            " 'face 'bold))
  (awhen (gethash "id" crate-data)
    (unless (eq it :null)
      (insert (propertize (number-to-string (floor it)) 'face 'marginalia-number))))
  (insert "\n\n")
  ;; (insert-crate-structure)
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (read-only-mode 1)
  )

;;;###autoload
(defun find-crate (name)
  (interactive "MRust Crate Name: ")
  (when (string-prefix-p "https://crates.io/crates/" name)
    (setq name (string-remove-prefix "https://crates.io/crates/" name)))
  (setq name (string-replace "-" "_" name))
  (let ((bufname (format "Crate: %s" name)))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (switch-to-buffer bufname)
      (setq-local crate-name name)
      (setq-local crate-data
                  ;; (or (gethash crate-name crate-foosoten)
                  ;;     (gethash (string-replace "_" "-" crate-name) crate-foosoten))
                  (or (gethash crate-name (crate-list-json))
                      (gethash (string-replace "_" "-" crate-name) (crate-list-json))))
      (crate-mode)))
  )

(keymap-global-set "s-±" #'find-crate)

;;;###autoload
(defun crate-browse-url (url &rest _args)
  (find-crate url))

;;;###autoload
(with-eval-after-load 'browse-url
  (add-to-list 'browse-url-default-handlers '("^https://crates\\.io/crates/" . crate-browse-url)))

(defun crate--bookmark-make-record-function ()
  "A function to be used as `bookmark-make-record-function'."
  `(,(concat "Rust Crate: " crate-name)
    (handler . crate-bookmark-jump)
    (crate . ,crate-name)
    ;; This location tag is used as a description in the bookmark menu list
    (location . ,(crate--description))
    ))

;;;###autoload
(defun crate-bookmark-jump (bm)
  "Jump to the crate bookmark BM."
  (interactive (list (read-from-minibuffer "Bookmark: ")))
  (let ((name (bookmark-prop-get bm 'crate)))
    (find-crate name)))
(put 'crate-bookmark-jump 'bookmark-handler-type "Crate")

(defun crate--org-store-link (_interactive-p)
  (when (eq major-mode 'crate-mode)
    (org-link-store-props :type "crate"
                          :link (format "crate:%s" crate-name)
                          :description
                          (format "Rust Crate: %s" crate-name))
    t))

(with-eval-after-load 'ol
  (org-link-set-parameters "crate"
                           :follow #'find-crate
                           :store #'crate--org-store-link))

(provide 'crate)
;;; crate.el ends here
