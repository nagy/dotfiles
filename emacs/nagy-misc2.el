;;; nagy-misc2.el --- More miscellaneous settings -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter org-pdftools nagy-use-package)

(require 'diminish)

;; NIX-EMACS-PACKAGE: dash
(require 'dash)
(require 'general)
(require 'eieio)

;; (defun pdf-crop-file ()
;;   (interactive)
;;   (alet (concat (make-temp-file "PDFCROPPED" t)
;;                 "/"
;;                 (file-name-nondirectory (buffer-file-name)))
;;     (cl-assert (zerop (call-process "pdf-crop-margins" nil nil nil
;;                                     "--verbose"
;;                                     "--outfile" it
;;                                     "--percentRetain" "0"
;;                                     "--absoluteOffset" "-6"
;;                                     (buffer-file-name))))
;;     (find-file it)))

;; NIX-EMACS-PACKAGE: highlight-quoted
(use-package highlight-quoted
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

(defvar redshift-location "12.34:56.78")
(defun redshift ()
  (interactive)
  (if-let* ((it (get-process "redshift")))
      (interrupt-process it)
    (if (executable-find "redshift")
        (start-process "redshift" nil "redshift" "-l" redshift-location)
      (user-error "redshift not installed"))))
(keymap-global-set "H-<f4>" #'redshift)

(declare-function brightness-down "nagy-exwm")
(defun system-suspend ()
  (interactive)
  ;; (real-garbage-collect)
  (dotimes (_ 20)
    (brightness-down))
  (start-process "sleeping" nil "sh" "-c" "sleep 2 && systemctl suspend"))
(keymap-global-set "s-💤" #'system-suspend)

;; NIX-EMACS-PACKAGE: super-save
(use-package super-save
  :preface
  (defun nagy-super-save-predicate ()
    (string-prefix-p temporary-file-directory default-directory))
  :demand t
  :diminish super-save-mode
  :commands (super-save-mode)
  ;; :custom
  ;; (super-save-auto-save-when-idle t)
  :config
  (add-to-list 'super-save-predicates #'nagy-super-save-predicate 'append)
  (add-to-list 'super-save-triggers 'previous-window-any-frame)
  (add-to-list 'super-save-triggers 'next-window-any-frame)
  (add-to-list 'super-save-triggers 'delete-window-or-tab)
  (add-to-list 'super-save-triggers 'silent-tab-next)
  (add-to-list 'super-save-triggers 'silent-tab-previous)
  (super-save-mode 1))

;; TODO carry over doom autoload
;; NIX-EMACS-PACKAGE: highlight-defined
(use-package highlight-defined
  :preface
  ;; copied from doom
  (defun ad-get-orig-definition (function) ;FIXME: Rename to "-unadvised-".
    (if (symbolp function)
        (setq function (if (fboundp function)
                           (advice--strip-macro (symbol-function function)))))
    (advice--cd*r function))
  (defvar +emacs-lisp--face nil)
  (defun +emacs-lisp-highlight-vars-and-faces (end)
    (catch 'matcher
      (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
        (let ((ppss (save-excursion (syntax-ppss))))
          (cond ((nth 3 ppss)           ; strings
                 (search-forward "\"" end t))
                ((nth 4 ppss)           ; comments
                 (forward-line +1))
                ((let ((symbol (intern-soft (match-string-no-properties 0))))
                   (and (cond ((null symbol) nil)
                              ((eq symbol t) nil)
                              ((keywordp symbol) nil)
                              ((special-variable-p symbol)
                               (setq +emacs-lisp--face 'font-lock-variable-name-face))
                              ((and (fboundp symbol)
                                    (eq (char-before (match-beginning 0)) ?\()
                                    (not (memq (char-before (1- (match-beginning 0)))
                                               (list ?\' ?\`))))
                               (let ((unaliased (indirect-function symbol)))
                                 (unless (or (macrop unaliased)
                                             (special-form-p unaliased))
                                   (let (unadvised)
                                     (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                     (setq unaliased (indirect-function unadvised)))))
                                     unaliased)
                                   (setq +emacs-lisp--face
                                         (if (subrp unaliased)
                                             'font-lock-constant-face
                                           'font-lock-function-name-face))))))
                        (throw 'matcher t)))))))
      nil))
  (defun nagy-misc2-activate-el-fl ()
    (font-lock-add-keywords
     'emacs-lisp-mode
     (append ;; highlight defined, special variables & functions
      `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))

  :config
  (list)
  :hook
  (emacs-lisp-mode-hook . nagy-misc2-activate-el-fl))
;; (emacs-lisp-mode-hook . highlight-defined-mode)


;; NIX-EMACS-PACKAGE: cyphejor
(use-package cyphejor
  :preface
  :commands (cyphejor-mode)
  :demand t
  :custom
  (cyphejor-rules '(;; :upcase
                    ("dired" "δ")
                    ("emacs lisp" "λ")
                    ("nagy-list" "ł")))

  :config
  (defun cyphejor--cypher (_old-name _rules)
    (when-let* ((it (format-mode-line mode-name)))
      (pcase it
        ((prefix "Dired") (string-replace "Dired" "δ" it))
        ((prefix "ELisp") (string-replace "ELisp" "𝛌" it))
        ("IELM" (string-replace "IELM" "𝛌" it))
        ((prefix "Magit") (string-replace "Magit" "" it))
        ((prefix "EMMS") (string-replace "EMMS" "󰎈" it))
        ((prefix "Nix") (string-replace "Nix" "○" it))
        ((prefix "Shell") (string-replace "Shell" "$" it))
        ((prefix "Inferior Python") (string-replace "Inferior Python" "↓🐍" it))
        ((prefix "Inferior Hy") (string-replace "Inferior Hy" "↓Hy" it))
        ((prefix "nagy-list") (string-replace "nagy-list" "ł" it))
        ((prefix "Text") (string-replace "Text" "T" it))
        ((prefix "Fundamental") (string-replace "Fundamental" "_" it))
        (_ mode-name)))))
;; in emacs 30, this can only be activated later ( maybe after
;; emacs-lisp-mode has been loaded).
;; (cyphejor-mode 1)


;; (use-package tokei
;;   :disabled
;;   ;; :config
;;   ;; (after! consult-imenu
;;   ;;         (add-to-list 'consult-imenu-config
;;   ;;                      '(tokei-mode
;;   ;;                        :toplevel "Languages"
;;   ;;                        :types ((?l "Languages"  magit-section-heading)
;;   ;;                                (?f "Files")))))
;;   )

;; (evil-collection-init 'pdf)
;; (evil-collection-pdf-setup)
;; (evil-set-initial-state 'pdf-view-mode 'normal)


(defun take-screenshot ()
  (interactive)
  (let* ((default-directory temporary-file-directory)
         (filename (string-trim-right
                    (shell-command-to-string "scrot --select --exec 'echo $f'"))))
    (if (called-interactively-p 'interactive)
        (find-file filename)
      (expand-file-name filename))))

;; (use-package jit-lock
;;   ;; https://old.reddit.com/r/emacs/comments/14c4l8j/way_to_make_emacs_feel_smoother/joku4bh/
;;   :custom
;;   (jit-lock-stealth-time 1.25)
;;   (jit-lock-stealth-nice 0.5)
;;   (jit-lock-chunk-size 4096))

;; NIX-EMACS-PACKAGE: avy
(use-package avy
  :defer t
  :custom
  (avy-all-windows nil)
  (avy-background nil)
  ;; (avy-single-candidate-jump t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l
                 ?q ?w ?e ?r       ?i ?o ?p)))

;; (use-package which-key
;;   :bind
;;   ("A-C-s-ĸ" . which-key-mode)
;;   :init
;;   (add-hook! which-key-mode
;;     (unless which-key-mode
;;       ;; Somehow it does not get set back
;;       ;; Still not good; embark as well
;;       (setq echo-keystrokes 0.02)))
;;   :custom
;;   (which-key-idle-delay 0))

(require 'magit-section)
;; NIX-EMACS-PACKAGE: bufler
(use-package bufler
  :defer t
  :commands (bufler-define-buffer-command bufler--map-sections)
  :custom
  ;; Remove buffer mode annotation
  (bufler-buffer-mode-annotate-preds nil)
  :config
  ;; Remove bufler column "VC". might be expensive
  (setq bufler-columns (remove "VC" bufler-columns))
  (bufler-define-buffer-command simple-switch "Simple switcher"
    #'switch-to-buffer :refresh-p nil))


;; NIX-EMACS-PACKAGE: iedit
(use-package iedit
  :bind
  ("C-;" . iedit-mode))

;; NIX-EMACS-PACKAGE: expand-region
(use-package expand-region
  :bind
  ("M-→" . er/expand-region)
  ("M-←" . er/contract-region))

;; NIX-EMACS-PACKAGE: page-break-lines
(use-package page-break-lines
  :diminish page-break-lines-mode
  :commands (global-page-break-lines-mode)
  :custom-face
  (page-break-lines ((t (:inherit parenthesis :foreground unspecified))))
  :custom
  (page-break-lines-modes '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode
                                            outline-mode help-mode text-mode conf-mode
                                            forth-mode))
  :config
  (global-page-break-lines-mode 1))


;; NIX-EMACS-PACKAGE: beginend
(use-package beginend
  ;; :diminish beginend-global-mode
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode))
  (beginend-global-mode)
  :bind
  ("H-<up>" . beginning-of-buffer)
  ("H-<down>" . end-of-buffer))

;; NIX-EMACS-PACKAGE: emacspy
;; (use-package emacspy)

;; NIX-EMACS-PACKAGE: llama
(use-package llama)
;; :config
;; (llama-fontify-mode)


;; ;; NIX-EMACS-PACKAGE: sparql-mode
;; (use-package sparql-mode
;;   :defer t
;;   :mode "\\.sparql\\'"
;;   )


;; NIX-EMACS-PACKAGE: treesit-fold
(use-package treesit-fold
  :defer t
  :custom
  (treesit-fold-line-count-show t)
  ;; (treesit-fold-summary-show t)
  :config
  (add-hook 'rustic-mode-hook #'treesit-fold-mode)
  (add-hook 'rustic-mode-hook (lambda () (treesit-parser-create 'rust)))
  (add-hook 'emacs-lisp-mode-hook #'treesit-fold-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda () (treesit-parser-create 'elisp)))
  (add-hook 'typescript-mode-hook #'treesit-fold-mode)
  (add-hook 'typescript-mode-hook (lambda () (treesit-parser-create 'typescript)))
  :bind
  ("H-i" . treesit-fold-close)
  ("H-I" . treesit-fold-close-all)
  ("H-o" . treesit-fold-open)
  ("H-O" . treesit-fold-open-all)
  ("C-H-o" . treesit-fold-open-recursively))


(use-package sh-script
  :preface
  (reformatter-define shfmt
    :group 'emacs
    ;; :lighter " shfmt"
    :program "shfmt"
    :args '("-i" "2" "--case-indent" "--space-redirects"))

  :hook
  (sh-mode-hook . shfmt-on-save-mode)
  :general
  (:states 'normal :keymaps 'sh-mode-map
           "⊢" #'shfmt-buffer))

;; https://github.com/rejeep/ansi.el
;; NIX-EMACS-PACKAGE: ansi
(use-package ansi
  :defer t)

;; NIX-EMACS-PACKAGE: yublin
(use-package yublin
  :defer t
  ;; :diminish yublin-mode
  ;; extras:
  ;; pnal -> personal
  ;; kjs -> changes
  ;; exion -> exception
  ;; lks  -> looks
  ;; tking  -> thinking
  :hook
  (text-mode-hook . yublin-mode))

;; https://github.com/alberti42/pretty-tables.el
;; One Nix package holds `pretty-tables', `pretty-tables-for-org' and
;; `pretty-tables-for-markdown'.
;; NIX-EMACS-PACKAGE: pretty-tables
(use-package pretty-tables
  :defer t
  :custom
  ;; Maximum table width.  Nil means `fill-column'.
  (pretty-tables-width nil)
  (pretty-tables-min-column-width 8)
  (pretty-tables-stripe-rows t))

(use-package pretty-tables-for-org
  :defer t
  :hook
  (org-mode-hook . pretty-tables-for-org-mode))

(use-package pretty-tables-for-markdown
  :defer t
  :hook
  (markdown-ts-mode-hook . pretty-tables-for-markdown-mode))

;; ;; NIX-EMACS-PACKAGE: banking
;; (use-package banking
;;   :defer t)

(provide 'nagy-misc2)
;;; nagy-misc.el ends here
