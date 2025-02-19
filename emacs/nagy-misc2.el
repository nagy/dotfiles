;;; nagy-misc2.el --- Description -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;;
;; Copyright (C) 2023 Daniel Nagy
;;
;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Maintainer: Daniel Nagy <danielnagy@posteo.de>
;; Created: March 03, 2023
;; Modified: March 03, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagy/nagy-misc2
;; Package-Requires: ((emacs "29.1") dash smartparens aggressive-indent ace-window reformatter browse-at-remote inspector highlight-quoted pass password-store password-store-otp expand-region emms super-save bufler pdf-tools org-pdftools highlight-defined cyphejor avy helpful page-break-lines which-key iedit go-mode anaphora general nagy-use-package)
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'diminish)

(require 'dash)
(require 'general)
(require 'smartparens)

(use-package conf-mode
  :preface
  (reformatter-define taplofmt
    :group 'conf
    :program "taplo"
    :args `("fmt" "-"))
  (defun nagy-misc2-conf-space-mode-hook ()
    (setq-local outline-regexp "#\\{2,3\\} ")
    (setq-local outline-heading-alist '(("## " . 1) ("### " . 2)))
    (outline-minor-mode 1)              ; to recalculate the buttons
    )
  :bind
  ("H-M-T" . conf-toml-mode)
  (:map conf-mode-map
        ("H-j" . forward-paragraph)
        ("H-k" . backward-paragraph)
        ("C-⊢" . taplofmt-buffer)
        )
  :general
  (:states 'normal :keymaps 'conf-toml-mode-map
           "ö" #'save-buffer
           "⊢" #'taplofmt-buffer
           )
  :hook
  (conf-toml-mode . taplofmt-on-save-mode)
  (conf-space-mode . nagy-misc2-conf-space-mode-hook)
  :pretty 'conf-toml-mode
  ("true" . true) ("false" . false)
  :cycle 'conf-toml-mode
  ("true" "false"))

(use-package ace-window
  :init
  (setq aw-keys (list ?a ?s ?d ?f ?h ?j ?k ?l ))
  (setq aw-background nil)
  (setq aw-scope 'visible)
  (setq aw-leading-char-style 'path)
  :bind
  ("s-'" . ace-window))

(use-package aggressive-indent
  :diminish 'aggressive-indent-mode
  :custom
  (aggressive-indent-sit-for-time 0.5)
  :general
  (:states 'normal :keymaps 'prog-mode-map
           "«" #'aggressive-indent-mode))

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

(defvar-local nagy-misc2-browse-at-remote--fixed-url nil)
(use-package browse-at-remote
  :commands (browse-at-remote-get-url)
  :preface
  (defun nagy-misc2-browse-at-remote-kill-print ()
    (interactive)
    (awhen (or nagy-misc2-browse-at-remote--fixed-url
               (ignore-errors (browse-at-remote-get-url)))
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
  ("H-s-y" . nagy-misc2-browse-at-remote-kill-print))

(use-package inspector
  :custom
  (inspector-truncation-limit 10000)
  (inspector-slice-size 10000)
  :general
  (:states 'normal :keymaps 'inspector-mode-map
           "C-o" #'inspector-pop)
  :same
  "^\\*inspector\\*")

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(defun redshift ()
  (interactive)
  (aif (get-process "redshift")
      (interrupt-process it)
    (if (executable-find "redshift")
        (start-process "redshift" nil "redshift"
                       )
      (user-error "redshift not installed"))))
(keymap-global-set "H-<f3>" #'redshift)

(declare-function brightness-down "nagy-exwm")
(defun system-suspend ()
  (interactive)
  ;; (real-garbage-collect)
  (dolist (_ '(1 1 1 1 1 1 1 1 1))
    (brightness-down))
  (start-process "sleeping" nil "sh" "-c" "sleep 2 && systemctl suspend"))
(keymap-global-set "s-💤" #'system-suspend)

(use-package pass
  :custom
  (pass-username-field "Username")
  (pass-show-keybindings nil)
  :config
  ;; override. TODO turn this into advice :replace
  (defun pass-quit ()
    "Kill the buffer quitting the window."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pass-view-mode)
          (kill-buffer buf))))
    (quit-window t))
  :general
  (:states 'normal
           "🔑" #'pass)
  (:states 'normal :keymaps 'pass-mode-map
           "u" #'pass-copy-url))

(use-package password-store
  :preface
  (defun nagy-replace-sleep-for-with-sit-for (orig-fun &rest args)
    "Advice that replaces calls to `sleep-for'
with `sit-for'.

This can be sometimes useful if a function is hanging because it
waits for input."
    (cl-letf (((symbol-function 'sleep-for) #'sit-for))
      (apply orig-fun args)))
  :custom
  (password-store-time-before-clipboard-restore 10)
  (password-store-url-field "Url")
  :config
  ;; TODO PR this upstream into the mailing list
  ;; https://github.com/zx2c4/password-store
  ;; https://lists.zx2c4.com/mailman/listinfo/password-store
  (advice-add 'password-store--run :around #'nagy-replace-sleep-for-with-sit-for)
  :bind
  ("M-s-p" . password-store-copy)
  :general
  (:states 'normal :keymaps 'pass-mode-map
           "j" #'pass-next-entry
           "k" #'pass-prev-entry
           "o" #'pass-otp-options
           "w" #'pass-copy
           "b" #'pass-copy-username
           "f" #'pass-view)
  :same "^\\*Password-Store\\*$")

(use-package password-store-otp
  :defer t
  ;; to add autoloads
  :commands (password-store-otp-token)
  :config
  ;; TODO pr this upstream
  (advice-add 'password-store-otp-token :filter-return #'string-trim-right))

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
  (push 'previous-window-any-frame super-save-triggers)
  (push 'next-window-any-frame super-save-triggers)
  (push 'delete-window-or-tab super-save-triggers)
  (push 'silent-tab-next super-save-triggers)
  (push 'silent-tab-previous super-save-triggers)
  (super-save-mode 1))

;; TODO carry over doom autoload
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
      `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face))))
    )
  :config
  (list)
  :hook
  (emacs-lisp-mode . nagy-misc2-activate-el-fl)
  ;; (emacs-lisp-mode . highlight-defined-mode)
  )

(use-package helpful
  :commands (helpful--navigate)
  :preface
  (defun helpful--all-the-buttons ()
    (car (save-excursion
           (goto-char (point-min))
           (cl-loop for i from 1 to 1000
                    for btn = (forward-button 1 nil nil t)
                    when btn
                    when (string= "Navigate to definition" (button-get btn 'help-echo))
                    collect btn))))
  (defun helpful-jump-to-definition ()
    (interactive)
    (when-let ((btn (helpful--all-the-buttons)))
      (helpful--navigate btn)))
  :config
  (put 'helpful--bookmark-jump 'bookmark-handler-type "Helpful")
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
  (:map helpful-mode-map
        ([remap dired-jump] . helpful-jump-to-definition)))

(use-package cyphejor
  :preface
  :commands (cyphejor-mode)
  :demand t
  :custom
  (cyphejor-rules '(;; :upcase
                    ("dired" "δ")
                    ("emacs lisp" "λ")
                    ;; ("nagy-list" "ł")
                    ))
  :config
  (defun cyphejor--cypher (_old-name _rules)
    (awhen (format-mode-line mode-name)
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
        (_ mode-name))))
  ;; in emacs 30, this can only be activated later ( maybe after
  ;; emacs-lisp-mode has been loaded).
  ;; (cyphejor-mode 1)
  )

(use-package tokei
  :disabled
  :config
  (after! consult-imenu
          (add-to-list 'consult-imenu-config
                       '(tokei-mode
                         :toplevel "Languages"
                         :types ((?l "Languages"  magit-section-heading)
                                 (?f "Files"))))))

(declare-function image-mode-window-get "image-mode")
(defun print-dwim ()
  (interactive)
  (pcase major-mode
    ('pdf-view-mode
     (if (buffer-file-name)
         (call-process "lpr" nil nil nil "-o"
                       (format "page-ranges=%d"
                               (progn
                                 ;; this is just #'pdf-view-current-page inlined
                                 (image-mode-window-get 'page)))
                       (buffer-file-name))
       (user-error "No buffer file name for printing")))
    (_ (let ((file (make-temp-file "print-dwim" nil ".ps")))
         (if (region-active-p)
             (ps-print-region (region-beginning) (region-end) file)
           (ps-print-buffer file))
         ;; nix-build "<nixpkgs>" -A ghostscript
         (call-process "/nix/store/i8mvcnvy54izmkqlghb50a6pfa5z8qc4-ghostscript-with-X-10.04.0/bin/ps2pdf" nil nil nil "-sPAPERSIZE=a4" file "/tmp/printed.pdf")
         (find-file "/tmp/printed.pdf")))))
(keymap-global-set "<print>" #'print-dwim)

(defun nagy/pdf-to-text (&optional arg)
  (interactive "P")
  (let ((buf (generate-new-buffer "*pdftext*")))
    (if arg
        (call-process "pdftotext" nil buf nil (buffer-file-name) "-")
      (call-process "pdftotext" nil buf nil "-layout" "-nopgbrk"
                    ;; "-f" (number-to-string (pdf-view-current-page))
                    ;; "-l" (number-to-string (pdf-view-current-page))
                    (buffer-file-name)
                    "-"))
    (switch-to-buffer buf)
    (text-mode)
    (goto-char (point-min))))

(use-package pdf-tools
  :custom
  (pdf-view-use-scaling nil)
  (pdf-view-midnight-colors '("white" . "black"))
  :bind
  (:map pdf-view-mode-map
        ("H-j" . pdf-view-next-page-command)
        ("H-k" . pdf-view-previous-page-command)
        ("M-m" . pdf-view-midnight-minor-mode)
        )
  ;; :same
  ;; (rx bos "*Outline ")
  )

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

(use-package avy
  :defer t
  :custom
  (avy-all-windows nil)
  (avy-background nil)
  (avy-single-candidate-jump t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l
                 ?q ?w ?e ?r       ?i ?o ?p)))

;; TODO make H-< in normal mode: eval sexp and replace
;; TODO make H-< in vertico consult select to eval and replace the entry ( if buffer )
(defun nagy-eval-sexp-replace ()
  (interactive)
  (-let* ((forward-sexp-function #'sp-forward-sexp) ; fixes problems e.g. in yaml-mode
          ((beg . end) (or (bounds-of-thing-at-point 'sexp)
                           (save-mark-and-excursion
                             (mark-word)
                             (cons (region-beginning) (region-end)))))
          (str (buffer-substring-no-properties beg end))
          (expr (car (read-from-string str))))
    (atomic-change-group
      (save-excursion
        (let ((e (eval expr)))
          (delete-region beg end)
          (insert (format "%S" e)))))))
(keymap-global-set "H-<" #'nagy-eval-sexp-replace)

;; (defun +nagy/colorize ()
;;   (interactive)
;;   (let ((inhibit-read-only t)
;;         (inhibit-message t))
;;     (let ((evil-ex-current-buffer (current-buffer)))
;;       (save-excursion
;;         (evil-ex-execute "%s,,,g")))
;;     (ansi-color-apply-on-region (point-min) (point-max))
;;     (set-buffer-modified-p nil)))

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
                                #'switch-to-buffer :refresh-p nil)
  )

(use-package pdf-tools
  :commands (pdf-tools-install)
  :hook
  (pdf-view-mode . pdf-view-fit-page-to-window)
  :config
  (pdf-tools-install))

(use-package iedit
  :bind
  ("C-;" . iedit-mode))

(use-package expand-region
  :bind
  ("M-→" . er/expand-region)
  ("M-←" . er/contract-region))

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

;; (use-package smartparens
;;   :hook
;;   (emacs-lisp-mode . smartparens-mode)
;;   (ielm-mode . smartparens-mode))

(use-package go-mode
  :preface
  (reformatter-define go-fmt
    :group 'go
    :program "gofmt"
    :lighter " GF")
  :hook
  (go-mode . go-fmt-on-save-mode)
  :bind
  ("H-M-g" . go-mode))

(provide 'nagy-misc2)
;;; nagy-misc.el ends here
