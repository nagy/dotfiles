;;; nagy-vertico.el --- vertico config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") vertico embark consult consult-dir embark-consult marginalia orderless general nagy-evil nagy-use-package)

(require 'nagy-evil)

(require 'general)

(eval-when-compile
  (require 'consult))
(declare-function consult--customize-put "consult")
;; (require 'embark)
;; (require 'embark-consult)

(use-package vertico
  :commands (vertico-mode)
  :config
  (vertico-mode)
  :custom
  (vertico-cycle nil)
  (vertico-scroll-margin most-positive-fixnum)
  (vertico-count 7)
  (vertico-resize nil)
  :bind
  ("C-s--" . vertico-flat-mode)
  (:map vertico-map
        ("C-j"   . vertico-next)
        ("C-M-j" . vertico-next-group)
        ("C-k"   . vertico-previous)
        ("C-M-k" . vertico-previous-group)
        ;; ("C-n"   . vertico-next)
        ;; ("C-p"   . vertico-previous)
        ("<next>" . vertico-scroll-up)
        ("<prior>" . vertico-scroll-down)
        ("s-j"   . vertico-next)
        ("s-k"   . vertico-previous)
        ("H-j"   . vertico-next)
        ("H-k"   . vertico-previous))
  (:map minibuffer-local-map
        ("s--" . vertico-flat-mode)
        ("H-h" . delete-backward-char)))

(use-package vertico-quick
  :custom
  (vertico-quick1 "asdfghjklö")
  (vertico-quick2 "vbnm")
  :bind
  (:map vertico-map
        ("C-," . vertico-quick-exit)
        ("C-." . vertico-quick-exit)))

(use-package vertico-buffer
  :custom
  (vertico-buffer-display-action '(display-buffer-same-window))
  :general
  (:states 'normal
           "°" #'vertico-buffer-mode))

(use-package consult
  :defer t
  :custom
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (consult-line-start-from-top t)
  :general
  (:states 'normal
           "s" #'consult-line
           "Ø" #'consult-outline)
  :bind
  ("s-b" . consult-buffer)
  ("s-/" . consult-focus-lines)
  ("H-/" . consult-keep-lines)
  ("H-s" . consult-line)
  ("C-s-s" . consult-ripgrep)
  ([remap bookmark-jump] . consult-bookmar)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap Info-search] . consult-info)
  ([remap load-theme] . consult-theme)
  ([remap man] . consult-man)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap recentf-open-files] . consult-recent-file)
  :config
  (consult-customize
   consult-buffer
   :preview-key nil
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC"
   ;; Disable preview for `consult-theme' completely.
   consult-theme :preview-key nil))

(use-package consult-imenu
  :defer t
  :general
  (:states 'normal
           "ø" #'consult-imenu
           ;; "M-ø" #'imenu-list
           )
  :config
  (setq consult-imenu-config
        '((emacs-lisp-mode
           :toplevel "Section"
           :types ((?f "Functions"  font-lock-function-name-face)
                   (?m "Macros"    font-lock-function-name-face)
                   (?M "Major modes" font-lock-function-name-face)
                   (?p "Package"   font-lock-constant-face)
                   (?t "Types"     font-lock-type-face)
                   (?S "Section"   org-document-info)
                   (?v "Variables" font-lock-variable-name-face)))
          (lisp-mode
           :types ((?t "Types"      font-lock-type-face)
                   (?v "Variables"  font-lock-variable-name-face)))
          (magit-status-mode
           :toplevel "Staged changes"
           :types ((?s "Unstaged changes"  modus-themes-diff-removed)
                   (?s "Staged changes"  modus-themes-diff-added)
                   (?S "Stashes"        font-lock-function-name-face)
                   (?c "Recent commits"  modus-themes-diff-removed)))
          (magit-refs-mode
           :toplevel "Branches"
           :types ((?b "Branches"  modus-themes-diff-added)
                   (?t "Tags"     font-lock-function-name-face))))))

(use-package orderless
  :commands (orderless-define-completion-style)
  :preface
  ;; from doom
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  ;; (orderless-define-completion-style orderless+initialism

  ;;   (orderless-matching-styles '(orderless-initialism
  ;;                                orderless-literal
  ;;                                orderless-regexp)))
  ;; (push '(command (styles orderless+initialism)) completion-category-overrides)
  ;; (push '(variable (styles orderless+initialism)) completion-category-overrides)
  ;; (push '(symbol (styles orderless+initialism)) completion-category-overrides)
  )

(use-package marginalia
  :commands (marginalia-mode)
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode 1))

;; nice examples
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :defer t
  :custom
  (embark-confirm-act-all nil)
  (embark-mixed-indicator-delay most-positive-fixnum)
  ;; :preface
  ;; (defun nagy-vertico-embark-target-find-file-magit-section ()
  ;;   (when-let ((ret (magit-section-value-if 'file))
  ;;              (start (marker-position (oref (magit-current-section) start)))
  ;;              (end (marker-position (oref (magit-current-section) end))))
  ;;     `(file ,ret ,start . ,end)))
  ;; :config
  ;; (push #'nagy-vertico-embark-target-find-file-magit-section embark-target-finders)
  :config
  ;; (setq embark-indicators (delq 'embark-mixed-indicator embark-indicators))
  (keymap-set embark-url-map "<key-chord> f j" #'browse-url)
  (keymap-set embark-file-map "<key-chord> f j" #'find-file)
  ;; (require 'embark-org)
  ;; (keymap-set embark-org-link-map "<key-chord> f j" #'org-open-at-point)
  ;; (keymap-set emacs-lisp-mode-map "<normal-state> <key-chord> f h" #'embark-dwim)
  ;; (keymap-set emacs-lisp-mode-map "<normal-state> <key-chord> f j" #'embark-act)
  (keymap-set text-mode-map "<normal-state> <key-chord> f j" #'embark-act)
  (keymap-set text-mode-map "<normal-state> <key-chord> f h" #'embark-dwim)
  (keymap-set prog-mode-map "<normal-state> <key-chord> f j" #'embark-act)
  (keymap-set prog-mode-map "<normal-state> <key-chord> f h" #'embark-dwim)
  (keymap-set tabulated-list-mode-map "<normal-state> <key-chord> f j" #'embark-act)
  (keymap-set tabulated-list-mode-map "<normal-state> <key-chord> f h" #'embark-dwim)
  ;; (keymap-set conf-mode-map "<normal-state> <key-chord> f j" #'embark-act)
  ;; (keymap-set conf-mode-map "<normal-state> <key-chord> f h" #'embark-dwim)
  ;; (keymap-set helpful-mode-map "<normal-state> <key-chord> f j" #'embark-act)
  ;; (keymap-set helpful-mode-map "<normal-state> <key-chord> f h" #'embark-dwim)
  ;; (push '("^\\*Embark " display-buffer-same-window) display-buffer-alist)
  ;; :same "^\\*Embark "  ; this breaks the s-. key binding down below. I dont know why.
  :bind
  ("<XF86Paste>" . embark-act)
  ("C-<XF86Paste>" . embark-dwim)
  ("S-<XF86Paste>" . embark-act-all)
  ("A-<XF86Paste>" . embark-collect)
  (:map minibuffer-local-map
        ("H-m" . (lambda ()
                   (interactive)
                   (embark-select)
                   (vertico-next))))
  (:map embark-collect-mode-map
        ("H-m" . (lambda ()
                   (interactive)
                   (embark-select)
                   (forward-line))))
  (:map embark-collect-mode-map
        ("s-." . embark-export))
  (:map emacs-lisp-mode-map
        ("<normal-state> <key-chord> f j" . embark-act)
        ("<normal-state> <key-chord> f h" . embark-dwim))
  (:map minibuffer-mode-map
        ("s-." . embark-export)
        ("s-:" . embark-collect)
        ("s-<XF86Back>" . embark-live)))

(use-package consult-dir
  :bind
  ([remap list-directory] . consult-dir)
  (:map vertico-map
        ("C-x C-d" . consult-dir)
        ("C-x C-j" . consult-dir-jump-file)))

(provide 'nagy-vertico)
;;; nagy-vertico.el ends here
