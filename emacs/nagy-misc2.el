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
;; (emacs-lisp-mode-hook . highlight-defined-mode)


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

;; NIX-EMACS-PACKAGE: magit-section
(require 'magit-section)
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

;; ;; NIX-EMACS-PACKAGE: banking
;; (use-package banking
;;   :defer t)

(provide 'nagy-misc2)
;;; nagy-misc.el ends here
