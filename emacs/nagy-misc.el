;;; nagy-misc.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") ts ov paren-face systemd git-modes anaphora nagy-use-package)

(require 'ov)
(require 'general)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'nameless))

(use-package emacs
  :general
  (:states 'normal
           "²" #'duplicate-dwim))

;; NIX-EMACS-PACKAGE: nameless
(use-package nameless
  :diminish nameless-mode
  ;; :bind
  ;; TODO make this emacs mode only
  ;; ("<key-chord> - e" . nameless-mode)
  :general
  (:states 'insert :keymaps 'nameless-mode-map
           "s--" #'nameless-insert-name)
  :custom
  (nameless-prefix "─")
  (nameless-private-prefix t)
  (nameless-global-aliases
   '(("fl" . "font-lock")
     ("ms" . "magit-section")
     ("○" . "nix")
     ("□" . "blocker")
     ("▱" . "map")                      ; or 𝒎
     ("ℕd" . "nagy-data")
     ("ℕ" . "nagy")
     ("⧖" . "dired")
     ("ol" . "org-link")
     ("ox" . "org-export")
     ("ob" . "org-babel")
     ("o" . "org")
     ("ŧ" . "tokei")
     ("√" . "calc")
     ("e4" . "elforth"))))

;; TODO replace with golden mode
;; NIX-EMACS-PACKAGE: golden-ratio
(use-package golden-ratio
  :bind
  ("H-s-=" . golden-ratio-mode))

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
    (setq-local revert-buffer-function #'nagy-misc-eww-revert-buffer2)
    )
  (defun nagy-misc-eww-revert-buffer2 (&rest _args)
    (interactive)
    (eww-reload))
  :bind
  ("s-€" . eww)
  ("H-j" . scroll-up-command)
  ("H-k" . scroll-down-command)
  ;; :hook
  ;; (eww-mode . variable-pitch-mode)
  :general
  (:states 'normal :keymaps 'eww-mode-map
           "r" #'eww-reload
           "R" #'eww-readable
           "." #'eww-browse-with-external-browser)
  (:states 'motion :keymaps 'eww-mode-map
           [remap evil-jump-backward] #'eww-back-url
           [remap evil-jump-forward] #'eww-forward-url)
  :config
  (add-hook 'eww-mode-hook #'nagy-misc-eww-revert-buffer)
  )

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
      (copy-region-as-kill (region-beginning) (region-end))
      ;; use `lispyville-yank' here to trigger evil-goggles.
      ;; (lispyville-yank (region-beginning) (region-end) 'line )
      )))

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
        ("H-d" . nagy/delete-paragraph)
        ;; ("H-l" . magit-log-buffer-file)
        )
  :hook
  (prog-mode . visual-line-mode)
  :general
  (:states 'normal :keymaps 'prog-mode-map
           "ö" #'save-buffer))

;; NIX-EMACS-PACKAGE: tokei
(use-package tokei
  :bind
  ("M-⧖" . tokei)
  (:map tokei-mode-map
        ("H-l" . magit-log-all-branches))
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "⧖" #'tokei)
  )

;; NIX-EMACS-PACKAGE: wgrep
(use-package wgrep
  :bind
  (:map wgrep-mode-map
        ([remap kill-this-buffer] . wgrep-abort-changes)
        ([remap save-kill-buffer] . wgrep-finish-edit))
  :general
  (:states 'normal :keymaps 'wgrep-mode-map
           "ö" #'wgrep-finish-edit))

;; NIX-EMACS-PACKAGE: focus
(use-package focus
  :preface
  (defun nagy/fix-focus-face ()
    (set-face-attribute 'focus-unfocused nil :foreground 'unspecified :inherit 'parenthesis))
  :init
  (setq focus-mode-to-thing
        '((prog-mode . defun)
          (markdown-mode . paragraph)
          (elpher-mode . paragraph)
          (nix-mode . paragraph)
          (text-mode . sentence)))
  :commands focus-mode
  :bind
  ("H-M-f" . focus-mode)
  :config
  (add-hook 'modus-themes-after-load-theme-hook #'nagy/fix-focus-face))

;; NIX-EMACS-PACKAGE: eros
(use-package eros
  :commands (eros-mode)
  :custom
  (eros-eval-result-prefix "")
  :config
  (eros-mode 1))

(use-package cc-mode
  :defer t
  :pretty 'c-mode
  ("if" . if) ("else" . else) ("endif" . else)
  ("#define" . "»")
  ("#undef" . "«")
  ("void" . null)
  ("return" . return)
  ("while" . loop)
  ("const" . const))

(use-package calc
  :general
  (:states 'normal :keymaps 'calc-edit-mode-map
           "ö" #'calc-edit-finish)
  (:states 'normal :keymaps 'calc-mode-map
           "π" #'calc-pi))

(use-package wdired
  :general
  (:states 'normal :keymaps 'wdired-mode-map
           "ö" #'wdired-finish-edit))

(use-package image
  :custom
  (image-auto-resize 'fit-window)
  :general
  (:states 'normal :keymaps 'image-mode-map
           "P" #'image-transform-fit-to-window))

(use-package apropos
  :bind
  (:map apropos-mode-map
        ("H-j" . apropos-next-symbol)
        ("H-k" . apropos-previous-symbol))
  :general
  (:states 'normal :keymaps 'apropos-mode-map
           "f" #'apropos-follow))

(use-package gitconfig-mode
  :defer t
  :pretty 'gitconfig-mode
  ("true" . true) ("false" . false)
  ("branch" . "⌥"))

(use-package info
  :bind
  (:map Info-mode-map
        ("H-j" . Info-next)
        ("H-k" . Info-prev)
        )
  :general
  (:states 'normal :keymaps 'Info-mode-map
           "f" #'Info-follow-nearest-node
           "SPC" nil   ; was #'Info-scroll-up
           ))

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

(use-package comint
  ;; :preface
  ;; (defun +nagy/comint-delete-clear ()
  ;;   (interactive)
  ;;   (comint-delete-input)
  ;;   (comint-clear-buffer))
  ;; (defun nagy-comint-kill-buffer-h ()
  ;;   (when (and (process-live-p (get-buffer-process (current-buffer)))
  ;;              (derived-mode-p 'comint-mode))
  ;;     (comint-interrupt-subjob)))
  ;; (add-hook 'kill-buffer-hook #'nagy-comint-kill-buffer-h)
  :hook
  (comint-mode . visual-line-mode)
  :custom
  ;; can freeze emacs on something like sleep 10 if non-nil
  ;; https://old.reddit.com/r/emacs/comments/14377k9/weekly_tips_tricks_c_thread/jn8igpu/
  (comint-process-echoes nil)
  ;; (comint-move-point-for-output t)
  :bind
  (:map comint-mode-map
        ([remap revert-buffer-quick] . comint-clear-buffer)
        ("C-ö" . comint-next-input)
        ("H-Ö" . comint-previous-input)
        ("M-Ö" . comint-previous-input)
        ("H-d" . comint-send-eof)
        ("H-_" . comint-send-eof)
        ("H-j" . comint-next-prompt)
        ("H-k" . comint-previous-prompt)
        ;; ("<insert-state> <key-chord> f j" . comint-send-input)
        ("<normal-state> <key-chord> f h" . embark-dwim)
        ("<normal-state> <key-chord> f j" . embark-act)
        )
  :general
  (:states 'normal :keymaps 'comint-mode-map
           "Ö" #'comint-previous-input
           "ö" #'comint-send-input
           "_" #'comint-send-eof))

;; NIX-EMACS-PACKAGE: sotlisp
(use-package sotlisp
  :diminish sotlisp-mode
  :commands (speed-of-thought-mode)
  :config
  (speed-of-thought-mode -1)
  (speed-of-thought-mode 1)
  )

(use-package gitattributes-mode
  ;; also catch files in nix store
  :mode "-gitattributes\\'")

(use-package gitconfig-mode
  ;; also catch files in nix store
  :mode "-gitconfig\\'")

(use-package ielm
  :preface
  (defun ielm-on-buffer ()
    (interactive)
    (let* ((buf (current-buffer))
           (ielm-buf-name (concat "*ielm-" (buffer-name buf) "*")))
      (aif (get-buffer ielm-buf-name)
          (switch-to-buffer it)
        (ielm ielm-buf-name)
        (with-current-buffer ielm-buf-name
          (setq-local ielm-working-buffer buf)))))
  (defun nagy-misc-ielm-hook ()
    (setq mode-line-process '(":" (:eval (buffer-name ielm-working-buffer)))))
  ;; :config
  ;; (advice-add 'ielm-return :after #'evil-normal-state)
  :hook
  (inferior-emacs-lisp-mode . nagy-misc-ielm-hook)
  :general
  (:states 'normal :keymaps 'inferior-emacs-lisp-mode-map
           "ö" #'ielm-return)
  )

;; NIX-EMACS-PACKAGE: osm
(use-package osm
  :custom
  (osm-copyright nil)
  (osm-max-age nil)
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
  :same "^\\*osm")

;; NIX-EMACS-PACKAGE: literate-calc-mode
(use-package literate-calc-mode
  :defer t
  :custom
  (literate-calc-mode-idle-time .1)
  ;; (literate-calc-mode-idle-time nil)
  )

;; NIX-EMACS-PACKAGE: breadcrumb
(use-package breadcrumb
  :bind
  ("C-H-j" . breadcrumb-jump)
  :general
  (:states 'normal
           "¿" #'breadcrumb-mode))

(use-package tabulated-list
  :bind
  (:map tabulated-list-mode-map
        ("M-h" . tabulated-list-previous-column)
        ("M-l" . tabulated-list-next-column))
  :general
  (:states 'normal :keymaps 'tabulated-list-mode-map
           "{" #'tabulated-list-narrow-current-column
           "}" #'tabulated-list-widen-current-column
           "t" #'tabulated-list-sort
           ;; "i" #'tabulated-list-previous-column
           ;; "o" #'tabulated-list-next-column
           "i" #'previous-window-any-frame
           "o" #'next-window-any-frame
           ))

;; NIX-EMACS-PACKAGE: nhexl-mode
(use-package nhexl-mode
  :bind
  ("H-M-H" . nhexl-mode)
  ;; todo: upstream this into evil-collection
  :general
  (:states 'normal :keymaps 'nhexl-mode-map
           [remap evil-next-line] #'nhexl-next-line
           [remap evil-previous-line] #'nhexl-previous-line
           "0" #'nhexl-move-beginning-of-line
           "$" #'nhexl-move-end-of-line
           "w" #'nhexl-nibble-forward
           "b" #'nhexl-nibble-backward
           "gg" #'beginning-of-buffer
           "G" #'end-of-buffer)
  ;; (:states 'normal
  ;;          "⬡" #'nhexl-mode)
  )

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

;; NIX-EMACS-PACKAGE: devdocs
(use-package devdocs
  :custom
  (devdocs-use-mathjax nil)
  :same "^\\*devdocs\\*"
  :general
  (:states 'motion :keymaps 'devdocs-mode-map
           [remap evil-jump-backward] #'devdocs-go-back
           [remap evil-jump-forward] #'devdocs-go-forward))

;; NIX-EMACS-PACKAGE: poke-mode
(use-package poke-mode
  :defer t
  )

;; (add-hook 'python-mode-hook
;;           (lambda () (setq-local devdocs-current-docs '("python~3.13"))))

(provide 'nagy-misc)
;;; nagy-misc.el ends here
