;;; nagy-magit-signatures.el --- async cached commit signature check -*- lexical-binding: t; -*-

;; Git verifies every commit listed by "git log --show-signature", that is
;; one gpg or ssh-keygen process per commit, again on every refresh.
;;
;; This file does not use that argument.  After Magit refreshes a buffer,
;; it looks at the commit hashes that Magit already put there, paints them
;; from a session cache, and verifies the unknown ones as a background
;; job, in batches of `nagy-magit-signatures-batch-size'.  The job prints
;; the status characters that git would have printed.  Magit's own
;; signature faces apply, so there is nothing else to customize.
;;
;; A batch fails as a whole when one hash no longer resolves, for example
;; after a force-push.  Such a batch splits in halves, down to a single
;; hash, which becomes `magit-signature-error'.
;;
;; The cache lives for this Emacs session only and is keyed by the
;; abbreviated hash.

;; NIX-EMACS-PACKAGE: magit
(declare-function magit-parse-git-async "magit-process")
(declare-function transient-replace-suffix "transient")
(defvar magit-refresh-buffer-hook)
(defvar magit-log-show-signatures-limit)

(defcustom nagy-magit-signatures-show t
  "Whether to colour commit hashes by signature status.
Toggle this with \"-S\" in the log transient."
  :group 'magit
  :type 'boolean)

(defcustom nagy-magit-signatures-modes '(magit-log-mode magit-status-mode)
  "Major modes whose commit hashes get signature faces.
Derived modes, such as `magit-log-select-mode', are included."
  :group 'magit
  :type '(repeat symbol))

(defconst nagy-magit-signatures-batch-size 16
  "Number of hashes passed to one background verification job.")

(defconst nagy-magit-signatures-faces
  '((?G . magit-signature-good)        (?B . magit-signature-bad)
    (?U . magit-signature-untrusted)   (?X . magit-signature-expired)
    (?Y . magit-signature-expired-key) (?R . magit-signature-revoked)
    (?E . magit-signature-error)       (?N . magit-hash))
  "Map from signature status character to face.")

(defvar nagy-magit-signatures-cache (make-hash-table :test #'equal)
  "Map from abbreviated hash to signature status character.")

(defvar nagy-magit-signatures-pending (make-hash-table :test #'equal)
  "Set of abbreviated hashes with a verification job in flight.")

(defun nagy-magit-signatures--target-buffer-p ()
  "Return non-nil if the current mode is in `nagy-magit-signatures-modes'."
  (apply #'derived-mode-p nagy-magit-signatures-modes))

(defun nagy-magit-signatures--hashes ()
  "Return (ALL MISSING), the distinct abbreviated hashes in the buffer.
Hashes with a cached status or a job in flight are not MISSING."
  (let ((seen (make-hash-table :test #'equal))
        all missing match)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'font-lock-face 'magit-hash t))
        (let ((hash (buffer-substring-no-properties (prop-match-beginning match)
                                                    (prop-match-end match))))
          (unless (gethash hash seen)
            (puthash hash t seen)
            (push hash all)
            (unless (or (gethash hash nagy-magit-signatures-cache)
                        (gethash hash nagy-magit-signatures-pending))
              (push hash missing))))))
    (list all missing)))

(defun nagy-magit-signatures--fontify ()
  "Paint the hashes in the current buffer that have a cached status."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let (match)
        (while (setq match (text-property-search-forward 'font-lock-face 'magit-hash t))
          (let ((hash (buffer-substring-no-properties (prop-match-beginning match)
                                                      (prop-match-end match))))
            (when-let* ((status (gethash hash nagy-magit-signatures-cache)))
              (put-text-property (prop-match-beginning match) (prop-match-end match)
                                 'font-lock-face
                                 (alist-get status nagy-magit-signatures-faces)))))))))

(defun nagy-magit-signatures--unfontify ()
  "Restore the plain `magit-hash' face in the current buffer."
  (let ((inhibit-read-only t))
    (dolist (face (mapcar #'cdr nagy-magit-signatures-faces))
      (save-excursion
        (goto-char (point-min))
        (let (match)
          (while (setq match (text-property-search-forward 'font-lock-face face t))
            (put-text-property (prop-match-beginning match) (prop-match-end match)
                               'font-lock-face 'magit-hash)))))))

(defun nagy-magit-signatures--verify (hashes)
  "Start a background signature check for HASHES."
  (dolist (hash hashes)
    (puthash hash t nagy-magit-signatures-pending))
  (let ((process (magit-parse-git-async "log" "--no-walk=unsorted"
                                        "--format=%G?" hashes)))
    (process-put process 'hashes hashes)
    (set-process-sentinel process #'nagy-magit-signatures--sentinel)))

(defun nagy-magit-signatures--sentinel (process _event)
  (when (memq (process-status process) '(exit signal))
    (let* ((status-kind (process-status process))
           (hashes (process-get process 'hashes))
           (statuses (with-current-buffer (process-buffer process)
                       (split-string (buffer-string) "\n" t)))
           (failed (or (/= (process-exit-status process) 0)
                       (/= (length statuses) (length hashes))))
           (stderr (with-current-buffer (process-get process 'stderr-buf)
                     (string-trim (buffer-string))))
           (half (/ (length hashes) 2)))
      (kill-buffer (process-buffer process))
      (kill-buffer (process-get process 'stderr-buf))
      (dolist (hash hashes)
        (remhash hash nagy-magit-signatures-pending))
      (cond
       ;; A killed job, for example one that "q" aborted, leaves no
       ;; status.  Leave the hashes uncached, so a refresh retries.
       ((eq status-kind 'signal))
       ;; One unresolved hash, for example one that a force-push removed,
       ;; fails the whole batch.  Split, until the culprit stands alone.
       ((and failed (> (length hashes) 1))
        (nagy-magit-signatures--verify (seq-take hashes half))
        (nagy-magit-signatures--verify (nthcdr half hashes)))
       (failed
        (puthash (car hashes) ?E nagy-magit-signatures-cache)
        (message "Signature check failed: %s" stderr)
        (nagy-magit-signatures--sync-all))
       (t
        (seq-mapn (lambda (hash status)
                    (puthash hash (aref status 0) nagy-magit-signatures-cache))
                  hashes statuses)
        (nagy-magit-signatures--sync-all))))))

(defun nagy-magit-signatures--update ()
  "Paint cached statuses and start a job for each unknown hash."
  (pcase-let ((`(,all ,missing) (nagy-magit-signatures--hashes)))
    (nagy-magit-signatures--fontify)
    (cond ((null missing))
          ((> (length all) magit-log-show-signatures-limit)
           (message "Not checking %s signatures (see `%s')"
                    (length all) 'magit-log-show-signatures-limit))
          (t (while missing
               (nagy-magit-signatures--verify
                (seq-take missing nagy-magit-signatures-batch-size))
               (setq missing (nthcdr nagy-magit-signatures-batch-size missing)))))))

(defun nagy-magit-signatures--sync ()
  "Apply `nagy-magit-signatures-show' to the current buffer."
  (condition-case err
      (if nagy-magit-signatures-show
          (nagy-magit-signatures--update)
        (nagy-magit-signatures--unfontify))
    ;; Painting must never break a Magit refresh.
    (error (message "Signature colours: %S" err))))

(defun nagy-magit-signatures--sync-all ()
  "Apply `nagy-magit-signatures-show' to all Magit buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (nagy-magit-signatures--target-buffer-p)
        (nagy-magit-signatures--sync)))))

(defun nagy-magit-signatures--refresh ()
  "Paint the hashes of the buffer that Magit just refreshed."
  (when (nagy-magit-signatures--target-buffer-p)
    (nagy-magit-signatures--sync)))

(add-hook 'magit-refresh-buffer-hook #'nagy-magit-signatures--refresh)

(defun nagy-magit-signatures-toggle ()
  "Toggle signature colours in Magit buffers."
  (interactive)
  (setq nagy-magit-signatures-show (not nagy-magit-signatures-show))
  (nagy-magit-signatures--sync-all))

;; Magit stores the switch in a group that both `magit-log' and
;; `magit-log-refresh' include by name, so one replacement covers both.
;; The symbol below is the one that transient generates for that switch.
;; Loading transient here would cost half a second of startup time.
(with-eval-after-load 'magit-log
  (transient-replace-suffix
      'magit-log 'transient:magit-log-infix-arguments:--show-signature
    '("-S" (lambda ()
              (format "Commit signatures %s"
                      (if nagy-magit-signatures-show "on" "off")))
      nagy-magit-signatures-toggle :transient t)))

(provide 'nagy-magit-signatures)
;;; nagy-magit-signatures.el ends here
