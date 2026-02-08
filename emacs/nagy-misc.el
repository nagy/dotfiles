;;; nagy-misc.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter ts ov paren-face systemd git-modes nagy-use-package)

(require 'ov)
(require 'general)

;; NIX-EMACS-PACKAGE: anaphora
(require 'anaphora)

(eval-when-compile
  ;; To catch errors during batch compilation
  (require 'nameless))

(declare-function preload-url "local")

(use-package emacs
  :general
  (:states 'normal
           "²" #'duplicate-dwim))

(use-package autoinsert
  :defer t
  :custom
  (auto-insert-query nil)
  )

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
           "ö" #'save-buffer
           "ð" #'evil-delete-whole-line
           ))

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
        ("H-x" . comint-send-eof)
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
  :demand t
  :diminish sotlisp-mode
  :commands (speed-of-thought-mode)
  :config
  (with-eval-after-load 'evil
    (speed-of-thought-mode -1)
    (speed-of-thought-mode 1))
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
  :defer t
  :commands (nagy-devdocs-install)
  :custom
  (devdocs-use-mathjax nil)
  :config
  ;; replace
  (defun devdocs--available-docs ()
    (json-read-file (preload-url (format "%s/docs.json" devdocs-site-url))))
  (defun nagy-devdocs-install (orig-fun &rest args)
    (cl-letf (((symbol-function 'url-insert-file-contents) (lambda (url) (insert-file-contents (preload-url url)))))
      (apply orig-fun args)))
  (advice-add 'devdocs-install :around #'nagy-devdocs-install)
  ;; (advice-remove 'password-store--run #'nagy-devdocs-install)
  :same "^\\*devdocs\\*"
  :general
  (:states 'motion :keymaps 'devdocs-mode-map
           [remap evil-jump-backward] #'devdocs-go-back
           [remap evil-jump-forward] #'devdocs-go-forward))
;; (add-hook 'python-mode-hook
;;           (lambda () (setq-local devdocs-current-docs '("python~3.13"))))

;; NIX-EMACS-PACKAGE: poke-mode
(use-package poke-mode
  :defer t
  )

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
  ("H-M-O" . conf-toml-mode)
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

;; NIX-EMACS-PACKAGE: ace-window
(use-package ace-window
  :init
  (setq aw-keys (list ?a ?s ?d ?f ?h ?j ?k ?l ))
  (setq aw-background nil)
  (setq aw-scope 'visible)
  (setq aw-leading-char-style 'path)
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

;; * Zig

;; NIX-EMACS-PACKAGE: zig-mode
(use-package zig-mode
  :custom
  (zig-format-on-save nil)
  :pretty 'zig-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else) ("then" . then)
  ("fn" . def)
  ("const" . const)
  ("while" . loop)
  ("void" . null)
  ;; ("try" . try) ("catch" . except)
  ("pub" . "🌐")
  ("var" . "𝕧")
  ("bool" . "𝒃")
  ("struct" . "𝕤")
  ("return" . return)
  ("export" . export)
  :bind
  ("H-M-z" . zig-mode)
  (:map zig-mode-map
        ("C-⊢" . zig-format-buffer))
  :cycle 'zig-mode
  ("const" "var")
  :hook
  (zig-mode . zig-format-on-save-mode)
  :general
  (:states 'normal :keymaps 'zig-mode-map
           "⊢" #'zig-format-buffer))
;; (define-auto-insert
;;   `("\\.zig\\'" . "Zig skeleton")
;;    '("Short description: "
;;      "const std = @import(\"std\");" \n
;;      \n
;;      "pub fn main() !void {" \n
;;      "std.debug.print(\"Hello, World!\\n\", .{});" \n
;;      > _ \n
;;      "}" > \n))

;; * Rust

;; NIX-EMACS-PACKAGE: rustic
(use-package rustic
  :commands (rustic-setup-lsp)
  :preface
  (reformatter-define rustfmt
    :group 'rustic
    :program "rustfmt"
    :args `("--edition" "2024"))
  :hook
  (rustic-mode . rustfmt-on-save-mode)
  :mode
  ("Cargo\\.lock\\'" . conf-toml-mode)
  :bind
  ("H-M-r" . rustic-mode)
  (:map rustic-mode-map
        ("C-⊢" . rustfmt-buffer))
  :general
  (:states 'normal :keymaps 'rustic-mode-map
           "⊢" #'rustfmt-buffer)
  :config
  (require 'rustic-lsp)
  ;; (setq rustic-lsp-setup-p nil)
  (remove-hook 'rustic-mode-hook #'rustic-setup-lsp)
  ;; :config
  ;; (setq rustic-lsp-client 'eglot)
  :pretty 'rustic-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else)
  ("self" . self)
  ("fn" . def)
  ("new" . new)
  ("let" . let)
  ("mut" . "⌿")
  ("format!" . print)
  ("return" . return)
  ("assert_eq!" . assert)
  ("std" . stdlib)
  ("use" . import)
  ("enum" . "ⅇ")
  ("const" . const)
  ("pub" . "🌐")
  ("struct" . "𝕤")
  ("impl" . "𝕚")
  ("str" . "𝕤")
  ("Vec" . "𝕍")
  ("String" . "𝕊")
  ("Result" . "ℝ")
  ("Option" . "𝕆")
  ("unwrap" . "𝕌")
  :abbrev 'rustic-mode
  ("uw" . "unwrap")
  ;; ("rst" . "Result")
  ;; ("stg" . "String")
  ;; ("vc" . "Vec")
  ("l" . "let")
  ("m" . "mut")
  :cycle 'rustic-mode
  ("Result" "Option"))

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

;; * Python

(use-package python
  :preface
  ;; or use https://github.com/scop/emacs-ruff-format
  (reformatter-define ruff-format
    :group 'python
    :program "ruff"      ; needs ruff >= 0.1.2
    ;; :args `("format" "--stdin-filename" ,input-file "-")
    ;; it needs to reference to buffer-file-name to respect project settings
    :args `("format" "--stdin-filename" ,(or (buffer-file-name) input-file))
    )
  :hook
  (python-mode . ruff-format-on-save-mode)
  ;; (python-ts-mode . ruff-format-on-save-mode)
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  ;; This is disabled only to hide a warning.
  ;;
  ;; ⛔ Warning (python): Your ‘python-shell-interpreter’ doesn’t seem to support readline, yet ‘python-shell-completion-native-enable’ was t and "python3" is not part of the ‘python-shell-completion-native-disabled-interpreters’ list.  Native completions have been disabled locally. Consider installing the python package "readline".
  (python-shell-completion-native-enable nil)
  :pretty 'python-mode
  ("True" . true) ("False" . false)
  ("if" . if) ("else" . else)
  ("def" . def)
  ("class" . class)
  ("class" . defclass)
  ("raise" . throw)
  ("import" . import)
  ("try" . try) ("except" . except)
  ("return" . return)
  ("pass" . "…")
  ("self" . "▒")
  ("None" . null)
  ("not" . not)
  ("with" . [?↗ (Bl . Bl) ?↘])
  ("match" . "〣")
  :bind
  ("H-M-p" . python-mode)
  (:map python-mode-map
        ("C-⊢" . ruff-format-buffer))
  :general
  (:states 'normal
           "þ" #'run-python)
  (:states 'normal :keymaps 'python-mode-map
           "⊢" #'ruff-format-buffer)
  :cycle 'python-mode
  ("class" "def")
  ("str" "bytes")
  ("True" "False")
  :abbrev 'python-mode
  ("imp" . "import")
  :same "^\\*Python")

;; * Devops

;; NIX-EMACS-PACKAGE: dockerfile-mode
(use-package dockerfile-mode
  :defer t)

;; NIX-EMACS-PACKAGE: terraform-mode
(use-package terraform-mode
  :preface
  (reformatter-define terraform-fmt
    :program "tofu"
    :args '("fmt" "-")
    ;; :lighter " TFmt"
    :group 'terraform-mode)
  :defer t
  :pretty 'terraform-mode
  ("data" . [?𝒅 (Br . Bl) ?𝒂])
  ("provider" . [?𝒑 (Br . Bl) ?𝒓])
  ("resource" . [?𝒓 (Br . Bl) ?𝒆])
  ("output" . [?𝒐 (Br . Bl) ?𝒑])
  :general
  (:states 'normal :keymaps 'terraform-mode-map
           "⊢" #'terraform-fmt-buffer)
  :hook
  (terraform-mode . terraform-fmt-on-save-mode)
  ;; :config
  ;; (push '(terraform-mode "terraform-ls" "serve") eglot-server-programs)
  )

;; * Elfeed

;; NIX-EMACS-PACKAGE: elfeed
(use-package elfeed
  :preface
  (defvar elfeed-show-mode-hook nil)
  :defer t
  :custom
  (elfeed-show-truncate-long-urls nil)
  (elfeed-search-filter "+unread")
  (elfeed-search-title-max-width 100)
  (elfeed-curl-max-connections 1)
  :config
  (put 'elfeed-search-bookmark-handler 'bookmark-handler-type "Elfeed Search")
  ;; :bind
  ;; (:map elfeed-show-mode-map
  ;;       ("SPC" . nil))
  :general
  (:states 'normal :keymaps 'elfeed-search-mode-map
           "SPC" nil
           "r" #'elfeed-search-untag-all-unread
           "↓" #'elfeed-search-fetch)
  ;; (:states 'normal :keymaps 'elfeed-show-mode-map
  ;;          "SPC" nil)
  (:keymaps 'elfeed-search-mode-map
            [remap kill-this-buffer] #'elfeed-db-unload
            [remap save-kill-buffer] #'elfeed-db-unload
            [remap nagy-kill-this-buffer] #'elfeed-db-unload
            )
  ;; :same "^\\*elfeed-entry"
  )

;; * Haskell

;; NIX-EMACS-PACKAGE: haskell-mode
(use-package haskell-mode
  :defer t
  :defines (haskell-interactive-mode-map)
  ;; :config
  :bind
  (:map haskell-mode-map
        ("H-l" . haskell-process-load-file))
  (:map haskell-interactive-mode-map
        ([remap revert-buffer-quick] . haskell-interactive-mode-clear))
  )

;; NIX-EMACS-PACKAGE: ormolu
(use-package ormolu
  :defer t
  ;; :config
  )

;; * Corfu

;; NIX-EMACS-PACKAGE: corfu
(use-package corfu
  :defer t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-quit-at-boundary nil)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  :bind
  (:map corfu-map
        ("RET" . nil)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<key-chord> f j" . corfu-insert)))

;; * Elfeed

;; NIX-EMACS-PACKAGE: elpher
(use-package elpher
  :preface
  (declare-function bookmark-prop-get "bookmark")
  (defun elpher-bookmark-handler (record)
    (elpher-go (bookmark-prop-get record 'location)))
  (defun elpher-bookmark-make-record ()
    (cons "elpher"
          `((location . ,(url-recreate-url (cadr elpher-current-page)))
            (handler . elpher-bookmark-handler))))
  :commands (elpher-go)
  :functions (elpher-redraw)
  :custom
  (elpher-use-header nil)
  (elpher-ipv4-always t)
  (elpher-connection-timeout 10)
  :config
  (put 'elpher-bookmark-handler 'bookmark-handler-type "Elpher")
  (add-hook 'elpher-mode-hook
            (defun +nagy/elpher-hook ()
              (setq-local bookmark-make-record-function #'elpher-bookmark-make-record)
              ;; (setq-local revert-buffer-function (cmd! (elpher-redraw)))
              ))
  (set-face-attribute 'elpher-gemini-heading1 nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)
  (set-face-attribute 'elpher-gemini-heading2 nil :font "Et Bembo" :height 1.5 :inherit 'modus-themes-heading-2)
  (set-face-attribute 'elpher-gemini-heading3 nil :font "Et Bembo" :height 1.2 :inherit 'modus-themes-heading-3)
  (set-face-attribute 'elpher-gemini-preformatted nil :extend t :inherit 'nagy-nuanced-green)
  :general
  (:states 'normal :keymaps 'elpher-mode-map
           "q" #'quit-window
           "M-n" #'elpher-next-link
           "M-p" #'elpher-prev-link)
  (:states 'motion :keymaps 'elpher-mode-map
           ;; (define-key elpher-mode-map "f" nil) ; this might be needed
           "f" #'push-button
           "s" #'elpher-back
           "C-o" #'elpher-back
           ))

;; * Text

;; NIX-EMACS-PACKAGE: jinx
(use-package jinx
  :general
  (:states 'normal
           "×" #'jinx-mode))

;; NIX-EMACS-PACKAGE: wordnut
(use-package wordnut
  :defer t
  :same "^\\*WordNut\\*"
  :config
  ;; Disable header line
  (defun wordnut--headerline ()))

;; NIX-EMACS-PACKAGE: lorem-ipsum
(use-package lorem-ipsum
  :general
  (:states 'normal
           "C-🫧" #'lorem-ipsum-insert-sentences
           "🫧" #'lorem-ipsum-insert-paragraphs))

;; NIX-EMACS-PACKAGE: pandoc
(use-package pandoc
  :preface
  (defun nagy-text-to-plain ()
    (interactive)
    (pcase-exhaustive major-mode
      ('mhtml-mode
       (let ((buffer (generate-new-buffer (concat "Pandoc Plain: " (buffer-name)))))
         (call-process-region nil nil "pandoc" nil buffer nil
                              "--to=plain"
                              "--wrap=none"
                              "--from=html")
         (switch-to-buffer  buffer)
         (goto-char (point-min))
         (text-mode)
         ;; (olivetti-mode)
         ;; (let ((inhibit-message t))
         ;;   (olivetti-set-width 80))
         ))))
  ;; (evil-global-set-key 'motion (kbd "g H-M-t") #'nagy-text-to-plain)
  (defun nagy-text-to-org ()
    (interactive)
    (pcase-exhaustive major-mode
      ('mhtml-mode
       (let ((buffer (generate-new-buffer (concat "Pandoc Org: " (buffer-name)))))
         (call-process-region nil nil "pandoc" nil buffer nil
                              "--to=org"
                              "--wrap=none"
                              "--from=html")
         (switch-to-buffer  buffer)
         (goto-char (point-min))
         (org-mode)
         ))))
  ;; (evil-global-set-key 'motion (kbd "g H-M-o") #'nagy-text-to-org)
  )

;; TODO markdown disable toggle markup when pretty key is pressed

;; * Hy

;; NIX-EMACS-PACKAGE: hy-mode
(use-package hy-mode
  :config
  (setq hy-jedhy--enable? nil)
  :bind
  ("H-M-H" . hy-mode)
  :general
  (:states 'normal
           "Þ" #'run-hy)
  :pretty 'hy-mode
  ("True" . true) ("False" . false)
  ("import" . import)
  ("let" . let)
  ("setv" . setq)
  ("when" . when) ("unless" . unless)
  ("raise" . throw)
  ("len" . "≢")
  ("self" . "▒")
  ("defn" . def)
  ("defclass" . defclass)
  ("defmain" . "𝔐")
  ("with" . [?↗ (Bl . Bl) ?↘])
  ("it" . "✦")                          ; anaphoric
  ("ap-with" . [?↗ (Bl . Bl) ?↘])
  ("Path" . "𝕻")
  :abbrev 'hy-mode
  ("df" . "defn")
  ("dc" . "dict")
  ("sv" . "setv")
  ("wh" . "when")
  ("unl" . "unless")
  ("req" . "require")
  ("imp" . "import")
  :same
  "^\\*Hy\\*$"
  :hook
  (hy-mode . lispy-mode)
  )

(use-package hy-shell
  :defer t
  :preface
  (declare-function nagy-replace-switch-to-buffer-other-window "nagy-use-package")
  :config
  (advice-add 'run-hy :around #'nagy-replace-switch-to-buffer-other-window)
  ;; this does not work in :custom because it is a variable
  (setq hy-shell--interpreter-args nil)      ; remove --spy
  )

;; * Web

;; NIX-EMACS-PACKAGE: typescript-mode
(use-package typescript-mode
  :preface
  (reformatter-define deno-fmt
    :group 'emacs
    :program "deno"
    :args `("fmt" "-"))
  :defer t
  :pretty 'typescript-mode
  ("this" . self)
  ("import" . import)
  ("return" . return)
  ("new" . new)
  ("function" . def)
  ("const" . const)
  ("export" . export)
  ("string" . tostring)
  ("try" . try) ("catch" . except)
  :cycle 'typescript-mode
  ("let" "const")
  :general
  (:states 'normal :keymaps 'typescript-mode-map
           "⊢" #'deno-fmt-buffer)
  ;; :hook
  ;; (typescript-mode . deno-fmt-on-save-mode) ;; this breaks svelte mode down because that inherits typescript-mode
  )

(use-package js
  :preface
  (reformatter-define jq-format
    :group 'js
    :program "jq"
    :args '("--sort-keys")
    )
  :pretty 'js-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else)
  ("function" . def)
  ("return" . return)
  ("while" . loop)
  ("var" . let)
  ("const" . const)
  :bind
  ("H-M-j" . js-json-mode)
  (:map js-json-mode-map
        ("C-⊢" . jq-format-buffer))
  :hook
  (js-json-mode . jq-format-on-save-mode)
  :general
  (:states 'normal :keymaps 'js-json-mode-map
           "⊢" #'jq-format-buffer))

;; NIX-EMACS-PACKAGE: wat-mode
(use-package wat-mode
  ;; :preface
  ;; (reformatter-define wat-format
  ;;   :program "deno"
  ;;   :args `("run" "--allow-read" "npm:@webassemblyjs/wast-refmt" ,input-file))
  :defer t
  ;; until https://github.com/devonsparks/wat-mode/pull/3 is merged and the package is in melpa
  :commands (wat-mode)
  ;; until the package is in melpa
  :mode ("\\.wat\\'" . wat-mode)
  :config
  ;; This makes "wasm2wat" in `shell-command' buffers work.
  (add-to-list 'magic-fallback-mode-alist '("(module" . wat-mode))
  :pretty 'wat-mode
  ("export" . export)
  ("func" . def)
  ("type" . "ƭ")
  ("module" . "📦")
  ("global" . "🌐")
  ("memory" . "󰘨")
  ("table" . "󰣟")
  ("import" . import)
  :bind
  ("H-M-w" . wat-mode))

;; NIX-EMACS-PACKAGE: yaml-mode
(use-package yaml-mode
  :preface
  (reformatter-define yq-format
    :group 'js
    :program "yq"
    :args `("--prettyPrint" ,(or (buffer-file-name) input-file)))
  ;; :hook
  ;; (yaml-mode . yq-format-on-save-mode)
  :bind
  ("H-M-y" . yaml-mode)
  (:map yaml-mode-map
        ("C-⊢" . yq-format-buffer))
  :pretty 'yaml-mode
  ("true" . true) ("false" . false)
  :general
  (:states 'normal :keymaps 'yaml-mode-map
           "⊢" #'yq-format-buffer)
  )

;; NIX-EMACS-PACKAGE: jq-mode
(use-package jq-mode
  :preface
  (reformatter-define jqfmt
    :group 'emacs
    :program "jqfmt"
    :args '("-ob" "-ar" "-op" "pipe" ))
  :defer t
  ;; :mode "\\.jq\\'"
  ;; :interpreter "jq"
  ;; :hook
  ;; (jq-mode . jqfmt-on-save-mode)
  :bind
  (:map jq-mode-map
        ("C-⊢" . jqfmt-buffer))
  :general
  (:states 'normal :keymaps 'jq-mode-map
           "⊢" #'jqfmt-buffer)
  :pretty 'jq-mode
  ("def" . def)
  ("try" . try) ("catch" . except)
  :abbrev 'jq-mode
  ("d" . "def")
  ("t" . "try")
  ("c" . "catch")
  ("sel" . "select")
  ("con" . "contains"))

;; NIX-EMACS-PACKAGE: svelte-mode
(use-package svelte-mode
  :preface
  (reformatter-define deno-fmt-component
    :group 'emacs
    :program "deno"
    :stdin nil
    :stdout nil
    :input-file (reformatter-temp-file)
    :args `("fmt" "--unstable-component" ,input-file))
  :defer t
  :general
  (:states 'normal :keymaps 'svelte-mode-map
           "⊢" #'deno-fmt-component-buffer)
  :hook
  (svelte-mode . deno-fmt-component-on-save-mode)
  )

;; NIX-EMACS-PACKAGE: coffee-mode
(use-package coffee-mode
  :defer t
  :pretty 'coffee-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else))

;; * Typst

;; NIX-EMACS-PACKAGE: typst-ts-mode
(use-package typst-ts-mode
  :preface
  (reformatter-define typstyle
    :group 'emacs
    :program "typstyle"
    )
  :defer t
  :bind
  ("H-M-T" . typst-ts-mode)
  (:map typst-ts-mode-map
        ("C-⊢" . typstyle-buffer))
  :hook
  (typst-ts-mode . typstyle-on-save-mode)
  :general
  (:states 'normal :keymaps 'typst-ts-mode-map
           "⊢" #'typstyle-buffer)
  )

;; NIX-EMACS-PACKAGE: ox-typst
(use-package ox-typst
  :commands (org-typst-export-to-typst) ;; for autoload
  :after org
  :custom
  (org-typst-export-buffer-major-mode 'typst-ts-mode)
  ;; :config
  ;; (add-to-list 'org-export-options-alist
  ;;              '(:with-phone nil "phone" nil t))
  :bind
  (:map org-mode-map
        ("H-M-T" . org-typst-export-as-typst)
        ("H-M-P" . org-typst-export-to-pdf))
  )

;; ;; NIX-EMACS-PACKAGE: vlf
;; ;; https://elpa.gnu.org/packages/vlf.html
;; ;; https://github.com/m00natic/vlfi
;; (use-package vlf
;;   :defer t
;;   :custom
;;   ;; (vlf-application 'always)
;;   (vlf-batch-size (* 8 1024 1024))
;;   (vlf-tune-enabled nil)
;;   ;; :bind
;;   ;; (:map vlf-mode-map)
;;   )


;;  TODO integrate tinymist language server lsp https://github.com/Myriad-Dreamin/tinymist

;; NIX-EMACS-PACKAGE: csv-mode
(use-package csv-mode
  :defer t
  ;; :custom
  ;; (csv-align-style 'centre)
  ;; :config
  ;; (csv-align-mode)
  )

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
  ("H-s-w" . nagy-obvious-mode)
  )

;; ;; NIX-EMACS-PACKAGE: multiple-cursors
;; (use-package multiple-cursors
;;   :defer t
;;   :bind
;;   ("C-S-c C-S-c" . mc/edit-lines)
;;   )

(provide 'nagy-misc)
;;; nagy-misc.el ends here
