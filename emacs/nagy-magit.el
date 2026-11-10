;;; nagy-magit.el --- Magit configuration -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general)

(require 'general)

;; NIX-EMACS-PACKAGE: magit
(use-package magit
  :commands (magit-insert-worktrees)
  :custom
  (magit-pull-or-fetch t)
  (magit-no-confirm '(resurrect
                      discard
                      reverse))
  (magit-section-initial-visibility-alist '((untracked . show)
                                            (unstaged . show)
                                            ;; (unpushed . show) ;; is this the "Recent commits" section?
                                            (staged . show)
                                            (stashes . show)))
  :config
  ;; (remove-hook 'magit-status-sections-hook #'magit-insert-recent-commits)
  (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-worktrees nil t)
  (add-to-list 'display-buffer-alist '("^magit-revision" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^magit-stash" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^magit-process:" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^magit-diff:"  display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("^magit:" display-buffer-same-window))
  ;; Temporarily unset these two key because they interfere with ediff mode.
  (setf (alist-get 'magit-push transient-values)
        '("--force-with-lease"))
  (setf (alist-get 'magit-rebase transient-values)
        '("--autostash" "--committer-date-is-author-date"))
  ;; Enable "=S" (--show-signature) for all log commands.
  ;; `magit-log' and `magit-log-refresh' ignore the plain command keys.
  ;; Magit stores log args under `magit-log:<major-mode>'.
  (dolist (mode '(magit-log-mode magit-status-mode magit-log-select-mode))
    (setf (alist-get (intern (format "magit-log:%s" mode)) transient-values)
          (delete-dups
           (cons "--show-signature"
                 (get mode 'magit-log-default-arguments)))))
  ;; Signature check costs one gpg call per commit.
  ;; Default -n256 stays below this limit.
  (setq magit-log-show-signatures-limit 1000)
  ;; (transient-save-values)
  :bind
  ("H-g" . magit-status)
  ("H-L" . magit-log-buffer-file)
  ("H-R" . magit-show-refs)
  (:map magit-mode-map
        ;; ("H-b" . nagy-browse-url-of-buffer)
        ;; ("C-l" . magit-log-all-branches)
        ("H-L" . magit-log-all-branches)
        ("H-<" . magit-process-buffer)
        ("H-c" . magit-commit-create))
  (:map dired-mode-map
        ;; ("C-l" . magit-log-all-branches)
        ("H-l" . magit-log-current)
        ("H-L" . magit-log-all-branches)
        ("H-<" . magit-process-buffer))
  (:map magit-diff-mode-map
        ("SPC" . nil)) ;; was `scroll-up'
  (:map magit-log-select-mode-map
        ([remap save-kill-buffer] . magit-log-select-pick)
        ([remap kill-this-buffer] . magit-log-select-quit)
        ([remap nagy-kill-this-buffer] . magit-log-select-quit)))

;; NIX-EMACS-PACKAGE: magit-section
(use-package magit-section
  :general
  (:states 'normal :keymaps 'magit-section-mode-map
           ;; "r" #'magit-section-toggle  ; already rebase in magit itself
           "Ö" #'magit-section-cycle
           "ö" #'magit-section-toggle)
  :bind
  (:map magit-section-mode-map
        ("s-<kp-1>" . magit-section-show-level-1)
        ("s-<kp-2>" . magit-section-show-level-2)
        ("s-<kp-3>" . magit-section-show-level-3)
        ("s-<kp-4>" . magit-section-show-level-4)
        ("H-a" . magit-section-cycle)
        ("C-ö" . magit-section-cycle-global)
        ;; ("H-j" . magit-section-forward)
        ;; ("H-k" . magit-section-backward)
        ("<normal-state> <key-chord> f h" . embark-dwim)
        ("<normal-state> <key-chord> f j" . embark-act)))

;; NIX-EMACS-PACKAGE: with-editor
(use-package with-editor
  :bind
  (:map with-editor-mode-map
        ([remap save-kill-buffer] . with-editor-finish)
        ([remap kill-this-buffer] . with-editor-cancel)
        ([remap nagy-kill-this-buffer] . with-editor-cancel))
  :general
  (:states 'normal :keymaps 'with-editor-mode-map
           "ö" #'with-editor-finish))


;; NIX-EMACS-PACKAGE: browse-at-remote
(defvar-local nagy-magit-browse-at-remote--fixed-url nil)
(use-package browse-at-remote
  :commands (browse-at-remote-get-url)
  :preface
  (defun nagy-magit-browse-at-remote-kill-print ()
    (interactive)
    (when-let* ((it (or nagy-magit-browse-at-remote--fixed-url
                        (ignore-errors (browse-at-remote-get-url)))))
      (message "Copied: %S" it)
      (kill-new it)
      it))
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  :config
  ;; from doom:
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^gitlab\\." :type "gitlab") 'append)
  :bind
  ("C-H-s-y" . browse-at-remote)
  ("H-s-y" . nagy-magit-browse-at-remote-kill-print))

;; NIX-EMACS-PACKAGE: git-modes
(use-package gitattributes-mode
  ;; also catch files in nix store
  :mode "-gitattributes\\'")

;; NIX-EMACS-PACKAGE: git-modes
(use-package gitconfig-mode
  ;; also catch files in nix store
  :mode "-gitconfig\\'"
  :defer t)

(provide 'nagy-magit)
;;; nagy-magit.el ends here
