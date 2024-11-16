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
  (aggressive-indent-sit-for-time 0.1)
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

(defun system-suspend ()
  (interactive)
  ;; (real-garbage-collect)
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
           "🔑" #'pass))

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

(use-package tokei
  :disabled
  :config
  (after! consult-imenu
          (add-to-list 'consult-imenu-config
                       '(tokei-mode
                         :toplevel "Languages"
                         :types ((?l "Languages"  magit-section-heading)
                                 (?f "Files"))))))
(use-package pdf-tools
  :custom
  (pdf-view-use-scaling nil)
  (pdf-view-midnight-colors '("white" . "black"))
  :bind
  (:map pdf-view-mode-map
        ("H-j" . pdf-view-next-page-command)
        ("H-k" . pdf-view-previous-page-command)
        ("H-m" . pdf-view-midnight-minor-mode))
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
