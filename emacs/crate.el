;;; crate.el --- Rust crates -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "30.1"))

(require 'bookmark)

(declare-function org-link-set-parameters "ol")
(declare-function org-link-store-props "ol")
(declare-function ansi-color-apply-on-region "ansi-color")

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
      (with-temp-buffer
        (insert-file-contents crate-data-path)
        (goto-char (point-min))
        (json-parse-buffer)))))

(defvar crate-structure--cache (make-hash-table :test #'equal))

(defun crate-structure (name)
  (with-memoization (gethash name crate-structure--cache)
    (with-temp-buffer
      (let ((pkg-dir (string-replace "_" "-" name))
            (exitcode (call-process crate-modules-program nil t nil "structure" "--package" (string-replace "_" "-" name) "--lib")))
        ;; (message "ddir %s exitcode %d crate-name: %s" default-directory exitcode name)
        ;; the case in esp-hal crate
        (unless (eq 0 exitcode)
          (erase-buffer)
          (let ((default-directory pkg-dir))
            (call-process crate-modules-program nil t nil "structure" "--package" pkg-dir "--lib"))))
      ;; (call-process crate-modules-program nil t nil "structure" "--package" name "--lib")
      (string-remove-prefix "\n" (buffer-string)))))

(defun insert-crate-structure ()
  (let ((p (point)))
    (insert (crate-structure crate-name))
    (ansi-color-apply-on-region p (point))))


(defun crate--description ()
  (when-let* ((it (gethash "description" crate-data)))
    (setq it (string-replace "\n" "" it))
    (setq it (truncate-string-to-width it fill-column nil nil t))
    (if (eq it :null) "" it)))

(defun crate--insert-field (label key)
  "Insert LABEL, then the value of KEY from `crate-data'.
If the value is nil or :null, nothing is inserted after the label."
  (insert label)
  (let ((val (gethash key crate-data)))
    (unless (or (null val) (eq val :null))
      (insert val)))
  (insert "\n"))

(defvar crate-font-lock-keywords
  `(;; Field labels: "Name:", "Description:", etc. in bold
    ("^\\([A-Z][a-z]+:\\)[[:space:]]*"
     (1 'bold))
    ;; Crate name value
    ("^Name:[[:space:]]+\\(.+\\)"
     (1 'bold))
    ;; URLs on Homepage/Documentation/Repository lines
    ("^\\(?:Homepage\\|Documentation\\|Repository\\):[[:space:]]+\\(https?://[^[:space:]\n]+\\)"
     (1 'shr-link nil t))
    ;; Updated date
    ("^Updated:[[:space:]]+\\(.+\\)"
     (1 'marginalia-date))
    ;; Crate id number
    ("^Id:[[:space:]]+\\([0-9]+\\)"
     (1 'marginalia-number)))
  "Font-lock keywords for `crate-mode'.")

;;;###autoload
(define-derived-mode crate-mode text-mode "Crate"
  ;; (setq-local list-buffers-directory crate-name)
  (cd temporary-file-directory)
  (setq-local font-lock-defaults '(crate-font-lock-keywords))
  (setq-local bookmark-make-record-function #'crate--bookmark-make-record-function)
  (setq-local revert-buffer-function #'ignore)
  (setq-local url-knowledge-url (concat "https://crates.io/crates/" crate-name))
  (setq-local list-buffers-directory (gethash "description" crate-data))
  (insert "Name:          ")
  (insert crate-name "\n")
  (insert "Description:   ")
  (insert (crate--description))
  (insert "\n")
  ;; Repository (special: may cd into local checkout)
  (insert "Repository:    ")
  (when-let* ((it (gethash "repository" crate-data)))
    (unless (eq it :null)
      (insert (propertize it 'mouse-face 'highlight))
      (let ((filename (format "/mnt/archive/%s.git.sqfs/"
                              (string-replace "/" "__"
                                              (string-remove-prefix "https://" it)))))
        (when (file-exists-p filename)
          (cd filename)))))
  (insert "\n")
  (crate--insert-field "Homepage:      " "homepage")
  (crate--insert-field "Documentation: " "documentation")
  (crate--insert-field "Updated:       " "updated_at")
  (insert "Id:            ")
  (when-let* ((it (gethash "id" crate-data)))
    (unless (eq it :null)
      (insert (number-to-string (floor it)))))
  (insert "\n\n")
  ;; (insert-crate-structure)
  ;; Apply mouse-face to URLs (font-lock only handles the `face' property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "https?://[^[:space:]\n]+" nil t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           '(mouse-face highlight))))
  (font-lock-ensure)
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (read-only-mode 1))


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
      (crate-mode))))


;;;###autoload
(defun crate-browse-url (url &rest _args)
  (find-crate url))

;;;###autoload
(defun crate-install-browse-url-handler ()
  "Make `browse-url' open crates.io URLs with `find-crate'."
  (interactive)
  (with-eval-after-load 'browse-url
    (add-to-list 'browse-url-default-handlers
                 '("^https://crates\\.io/crates/" . crate-browse-url))))

(defun crate--bookmark-make-record-function ()
  "A function to be used as `bookmark-make-record-function'."
  `(,(concat "Rust Crate: " crate-name)
    (handler . crate-bookmark-jump)
    (crate . ,crate-name)
    ;; This location tag is used as a description in the bookmark menu list
    (location . ,(crate--description))))


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
