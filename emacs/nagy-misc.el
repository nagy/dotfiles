;;; nagy-misc.el --- Miscellaneous settings -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") ts ov paren-face systemd nagy-use-package)

(require 'ov)
(require 'general)

;; NIX-EMACS-PACKAGE: reformatter
;; (require 'reformatter)

(use-package emacs
  :general
  (:states 'normal
           "²" #'duplicate-dwim))

(use-package autoinsert
  :defer t
  :custom
  (auto-insert-query nil))

;; NIX-EMACS-PACKAGE: macrostep
(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
        ("<normal-state> µ" . macrostep-expand))
  (:map macrostep-mode-map
        ("c" . always)
        ("s-k" . macrostep-collapse-all)))

(defun nagy/delete-paragraph ()
  (interactive)
  (if (region-active-p)
      (progn
        (copy-region-as-kill (region-beginning) (region-end))
        (delete-region (region-beginning) (region-end)))
    (save-excursion
      (mark-paragraph)
      (copy-region-as-kill (region-beginning) (region-end))
      (delete-region (region-beginning) (region-end)))))
(defun nagy/yank-paragraph ()
  (interactive)
  (if (region-active-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (save-mark-and-excursion
      (mark-paragraph)
      (copy-region-as-kill (region-beginning) (region-end)))))
;; use `lispyville-yank' here to trigger evil-goggles.
;; (lispyville-yank (region-beginning) (region-end) 'line )

(use-package text-mode
  :bind
  (:map text-mode-map
        ("H-ö" . save-buffer)
        ("H-j" . forward-paragraph)
        ("H-k" . backward-paragraph)
        ("H-d" . nagy/delete-paragraph)
        ("H-y" . nagy/yank-paragraph))
  ;; :config
  ;; (with-eval-after-load 'magit
  ;;   (keymap-set text-mode-map
  ;;               "H-l" #'magit-log-buffer-file))
  :general
  (:states 'normal :keymaps 'text-mode-map
           "ö" #'save-buffer))

(use-package prog-mode
  :bind
  (:map prog-mode-map
        ("H-j" . end-of-defun)
        ("H-k" . beginning-of-defun)
        ("H-ö" . save-buffer)
        ("H-d" . nagy/delete-paragraph))
  ;; ("H-l" . magit-log-buffer-file)

  :hook
  (prog-mode-hook . visual-line-mode)
  (prog-mode-hook . display-line-numbers-mode)
  :general
  (:states 'normal :keymaps 'prog-mode-map
           "ö" #'save-buffer
           "ð" #'evil-delete-whole-line))

;; NIX-EMACS-PACKAGE: tokei
(use-package tokei
  :bind
  ("M-⧖" . tokei)
  (:map tokei-mode-map
        ("H-L" . magit-log-all-branches))
  :config
  (with-eval-after-load 'consult
    (defvar consult-bookmark-narrow)
    (push '(?t "Tokei" tokei-bookmark-jump) consult-bookmark-narrow))
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "⧖" #'tokei))

(use-package cc-mode
  :defer t)

(use-package image-mode
  :custom
  (image-auto-resize 'fit-window)
  :general
  (:states 'normal :keymaps 'image-mode-map
           "j" #'image-next-file
           "k" #'image-previous-file
           "O" #'image-transform-reset-to-original
           "P" #'image-transform-fit-to-window))

(use-package cus-edit
  :bind
  (:map custom-mode-map
        ([remap revert-buffer-quick] . Custom-reset-saved)
        ([remap save-buffer] . Custom-set)
        ([remap save-kill-buffer] . Custom-buffer-done))
  (:map custom-field-keymap
        ([remap revert-buffer-quick] . Custom-reset-saved)
        ([remap save-buffer] . Custom-set)
        ([remap save-kill-buffer] . Custom-buffer-done))
  :custom
  (custom-buffer-verbose-help nil)
  (custom-search-field nil)
  :general
  (:states 'normal :keymaps 'custom-mode-map
           "f" #'Custom-newline
           "u" #'Custom-goto-parent))

;; NIX-EMACS-PACKAGE: literate-calc-mode
(use-package literate-calc-mode
  :defer t
  :custom
  (literate-calc-mode-idle-time .1))
;; (literate-calc-mode-idle-time nil)

;; similar to doom defaults
;; (use-package macrostep
;;   :config
;;   (map! :n "µ" #'macrostep-expand))

;;;###autoload
(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block in seconds.
Returns the total execution time as a floating-point number."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

;; NIX-EMACS-PACKAGE: aggressive-indent
(define-auto-insert
  '("\\.rs\\'" . "Rust skeleton")
  '("Short description: "
    "use std::error::Error;" \n
    "use std::io;" \n
    \n
    "pub fn main() -> Result<(), Box<dyn Error>> {" \n
    "println!(\"Hello, World!\\n\");" \n
    > _ \n
    "Ok(())" \n
    "}" > \n))

;; NIX-EMACS-PACKAGE: obvious
;; ;; NIX-EMACS-PACKAGE: multiple-cursors
;; (use-package multiple-cursors
;;   :defer t
;;   :bind
;;   ("C-S-c C-S-c" . mc/edit-lines)
;;   )

(defun find-file-directory-yaml ()
  (interactive)
  (cond
   ((file-exists-p ".gitlab.yml") (find-file ".gitlab-ci.yml"))
   ((file-exists-p "kustomization.yaml") (find-file "kustomization.yaml"))
   (t (user-error "No useful YAML file found."))))

(require 'dired)
(keymap-set dired-mode-map "H-M-Y" #'find-file-directory-yaml)

;; ;; NIX-EMACS-PACKAGE: combobulate
;; (use-package combobulate
;; :defer t
;; :config
;; :bind
;; (:map prog-mode-map
;;       ("M-n" . combobulate-navigate-next)
;;       ("M-p" . combobulate-navigate-previous)
;;       ("M-k" . combobulate-kill-node-dwim)
;;       ("H-d" . combobulate-kill-node-dwim)
;;       )
;; )

(provide 'nagy-misc)
;;; nagy-misc.el ends here
