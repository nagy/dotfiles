;;; nagy-lispy.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-use-package)

(require 'diminish)

(require 'general)

;; NIX-EMACS-PACKAGE: lispy
(use-package lispy
  :diminish lispy-mode
  :commands (lispy-mode)
  :custom
  (lispy-completion-method 'default)
  ;; :config
  ;; Obscures key-chord
  ;; (map! :map lispy-mode-map
  ;;       "f" nil  ;; was special-lispy-flow
  ;;       )
  :hook
  (emacs-lisp-mode . lispy-mode)
  :bind
  ("s-(" . lispy-mode)
  ("s-)" . lispyville-mode)
  (:map lispy-mode-map
        ("H-x" . lispy-kill-at-point))
  (:map lispy-mode-map-special
        ("f" . nil)    ; disable lispy flow, interferes with key-chord
        )
  (:map lispy-mode-map-lispy
        ("]" . nil)
        ("[" . nil)
        ("C-," . nil)  ; was lispy-kill-at-point
        ))

;; NIX-EMACS-PACKAGE: lispyville
(use-package lispyville
  :commands (lispyville-set-key-theme)
  :diminish lispyville-mode
  :bind
  (:map lispyville-mode-map
        ("<normal-state> <key-chord> y SPC" . lispy-clone)
        ;; ("<normal-state> <key-chord> y ," . lispy-comment)
        ("<normal-state> <key-chord> f SPC" . lispy-raise-sexp)
        ("C-M-S-o" . lispy-oneline)
        ("M-S-RET" . lispy-multiline))
  :hook
  (lispy-mode . lispyville-mode)
  :general
  (:states 'normal :keymaps 'lispyville-mode-map
           "¢" #'lispy-clone
           "g C-j" #'lispy-down
           "g C-k" #'lispy-up
           ";" #'lispy-comment
           )
  ;; Imported from Doom.
  :init
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement t)
          slurp/barf-lispy
          additional
          additional-insert))
  :config
  (lispyville-set-key-theme)
  )

(provide 'nagy-lispy)
;;; nagy-lispy.el ends here
