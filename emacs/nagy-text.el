;;; nagy-text.el --- Text, prose and symbol display config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: jinx
(use-package jinx
  :general
  (:states 'normal
           "×" #'jinx-mode))

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

;; NIX-EMACS-PACKAGE: highlight-quoted
(use-package highlight-quoted
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

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
  (defun nagy-text-activate-el-fl ()
    (font-lock-add-keywords
     'emacs-lisp-mode
     (append ;; highlight defined, special variables & functions
      `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))

  :config
  (list)
  :hook
  (emacs-lisp-mode-hook . nagy-text-activate-el-fl))

(use-package word-wrap-mode
  :config
  (global-word-wrap-whitespace-mode 1))

(use-package string-edit
  :bind
  (:map string-edit-mode-map
        ([remap save-kill-buffer] . string-edit-done)
        ([remap kill-this-buffer] . string-edit-abort)
        ([remap nagy-kill-this-buffer] . string-edit-abort)))

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
;; TODO carry over doom autoload
;; (emacs-lisp-mode-hook . highlight-defined-mode)

(provide 'nagy-text)
;;; nagy-text.el ends here
