;;; nagy-prog.el --- Programming tooling config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package reformatter)

(require 'general)

(use-package compile
  ;; :custom
  ;; (compilation-scroll-output t)
  :bind
  ("C-M-¢" . compile)
  ("H-s-j" . next-error)
  ("H-s-k" . previous-error)
  ;; ("H-s-c" . compile)
  (:map compilation-mode-map
        ([remap revert-buffer-quick] . recompile))
  (:map compilation-minor-mode-map
        ([remap revert-buffer-quick] . recompile)
        ("H-j" . compilation-next-error)
        ("H-k" . compilation-previous-error)))

(use-package make-mode
  :defer t
  :bind
  ("H-M-M" . make-mode))

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

(use-package ediff
  :defer t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

;; https://github.com/rejeep/ansi.el
;; NIX-EMACS-PACKAGE: ansi
(use-package ansi
  :defer t)

(provide 'nagy-prog)
;;; nagy-prog.el ends here
