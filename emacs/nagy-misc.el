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

(use-package eww
  :preface
  (defun nagy-misc-eww-revert-buffer ()
    (interactive)
    (setq-local revert-buffer-function #'nagy-misc-eww-revert-buffer2))

  (defun nagy-misc-eww-revert-buffer2 (&rest _args)
    (interactive)
    (eww-reload))
  :bind
  ("s-€" . eww)
  ("H-j" . scroll-up-command)
  ("H-k" . scroll-down-command)
  ("H-y" . eww-copy-page-url)
  ;; :hook
  ;; (eww-mode-hook . variable-pitch-mode)
  :general
  (:states 'normal :keymaps 'eww-mode-map
           "r" #'eww-reload
           "R" #'eww-readable
           "." #'eww-browse-with-external-browser)
  (:states 'motion :keymaps 'eww-mode-map
           [remap evil-jump-backward] #'eww-back-url
           [remap evil-jump-forward] #'eww-forward-url)
  :config
  (add-hook 'eww-mode-hook #'nagy-misc-eww-revert-buffer))

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

;; NIX-EMACS-PACKAGE: focus
(use-package focus
  :preface
  (defun nagy/fix-focus-face ()
    (set-face-attribute 'focus-unfocused nil :foreground 'unspecified :inherit 'parenthesis))
  ;; (defun nagy/focus-tired-redraw ()
  ;;   ;; `tabulated-list-print' erases the listing, and the erase collapses
  ;;   ;; the three focus overlays, so a Tired print needs them back.
  ;;   (when (bound-and-true-p focus-mode)
  ;;     (focus-move-focus (current-buffer))))
  :init
  (setq focus-mode-to-thing
        '((prog-mode . defun)
          (markdown-ts-mode . paragraph)
          (elpher-mode . paragraph)
          (nix-mode . paragraph)
          (text-mode . sentence)))
          ;; The depth-zero entry of point with every row that it
          ;; prints below itself, however deep the cursor sits.
          ;; (tired-mode . tired-subtree)))
  :commands focus-mode
  :bind
  ("H-M-f" . focus-mode)
  :config
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/fix-focus-face))
  ;; (add-hook 'tired-after-redraw-hook #'nagy/focus-tired-redraw))

;; NIX-EMACS-PACKAGE: eros
(use-package eros
  :commands (eros-mode)
  :custom
  (eros-eval-result-prefix "")
  :config
  (eros-mode 1))

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

;; NIX-EMACS-PACKAGE: osm
(use-package osm
  :custom
  (osm-copyright nil)
  (osm-max-age nil)
  :config
  (with-eval-after-load 'consult
    (defvar consult-bookmark-narrow)
    (push '(?o "OSM" osm-bookmark-jump) consult-bookmark-narrow))
  :bind
  (:map osm-mode-map
        ("<home>" . osm-left-left)
        ("<end>" . osm-right-right)
        ("<next>" . osm-down-down)
        ("<prior>" . osm-up-up))
  ;; :config
  ;; (require 'osm-ol)
  ;; (put 'osm-bookmark-jump 'bookmark-handler-type "Osm")
  ;; (evil-set-initial-state 'osm-mode 'emacs)
  :init
  (nagy-same-window "^\\*osm"))

;; NIX-EMACS-PACKAGE: literate-calc-mode
(use-package literate-calc-mode
  :defer t
  :custom
  (literate-calc-mode-idle-time .1))
;; (literate-calc-mode-idle-time nil)

;; NIX-EMACS-PACKAGE: breadcrumb
(use-package breadcrumb
  :bind
  ("C-H-j" . breadcrumb-jump)
  :general
  (:states 'normal
           "¿" #'breadcrumb-mode))

;; similar to doom defaults
;; (use-package drag-stuff
;;   :defer t
;;   :init
;;   (map! "<H-up>"    #'drag-stuff-up
;;         "<H-down>"  #'drag-stuff-down
;;         "<H-left>"  #'drag-stuff-left
;;         "<H-right>" #'drag-stuff-right))

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

;; NIX-EMACS-PACKAGE: ace-window
(use-package ace-window
  :custom
  (aw-keys (list ?a ?s ?d ?f ?h ?j ?k ?l))
  (aw-background nil)
  (aw-scope 'visible)
  (aw-leading-char-style 'path)
  :bind
  ("s-'" . ace-window))

;; NIX-EMACS-PACKAGE: aggressive-indent
(use-package aggressive-indent
  :diminish 'aggressive-indent-mode
  :custom
  (aggressive-indent-sit-for-time 0.5)
  :general
  (:states 'normal :keymaps 'prog-mode-map
           "«" #'aggressive-indent-mode))

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

;; * Text

;; NIX-EMACS-PACKAGE: jinx
(use-package jinx
  :general
  (:states 'normal
           "×" #'jinx-mode))

(use-package xref
  :defer t
  :custom
  (xref-search-program 'ripgrep))

;; NIX-EMACS-PACKAGE: obvious
(use-package obvious
  :commands (nagy-obvious-mode)
  :defer t
  :custom
  (obvious-headers nil)
  ;; (obvious-preserve-blank-lines nil)
  :preface
  (defun nagy-obvious-mode (&optional arg)
    (interactive
     (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg)
             'toggle)))
    (read-only-mode arg)
    (obvious-mode arg))
  :bind
  ("<f12>" . nagy-obvious-mode)
  ("H-s-w" . nagy-obvious-mode))

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

;; NIX-EMACS-PACKAGE: drag-stuff
(use-package drag-stuff
  :defer t
  :bind
  ("M-k" . drag-stuff-up)
  ("M-j" . drag-stuff-down))
;; ("<M-left>"  . drag-stuff-left)
;; ("<M-right>" . drag-stuff-right)
;; :config
;; (define-key esc-map "j" nil)

;; NIX-EMACS-PACKAGE: ipfs-modes
(use-package cid-mode
  :defer t
  :preface
  (defun nagy-cid-mode-set-url ()
    (when cid--info
      (setq-local url-knowledge-url (concat "https://cid.ipfs.tech/#" (cid-info-cid-string cid--info)))))
  (defun nagy-cid-mode-cd ()
    (let ((path (concat "/tmp/ipfs/" (cid-info-cid-string cid--info))))
      (when (file-directory-p path)
        (cd path))))
  :init
  (nagy-same-window "^\\*CID: ")
  :hook
  (cid-decode-hook . nagy-cid-mode-set-url)
  (cid-decode-hook . nagy-cid-mode-cd)
  :bind
  (:map cid-mode-map
        ([emap eww-copy-page-url] . cid-copy-cid)))

(use-package embark-cid
  :defer t)

(provide 'nagy-misc)
;;; nagy-misc.el ends here
