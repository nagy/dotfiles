;;; nagy-lisp.el --- Generic Lisp config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-use-package)

(use-package scheme
  :bind
  (:map scheme-mode-map
        ("s-." . scheme-send-last-sexp)
        ("s--" . scheme-send-definition)))
  ;; :hook
  ;; (scheme-mode-hook . lispy-mode)

;; NIX-EMACS-PACKAGE: sotlisp
(use-package sotlisp
  :demand t
  :diminish sotlisp-mode
  :commands (speed-of-thought-mode)
  :config
  (with-eval-after-load 'evil
    (speed-of-thought-mode -1)
    (speed-of-thought-mode 1)))

;; NIX-EMACS-PACKAGE: parinfer-rust-mode
(use-package parinfer-rust-mode
  :defer t
  :preface
  :hook
  (emacs-lisp-mode-hook . parinfer-rust-mode)
  :config
  (require 'xdg)
  :custom
  (parinfer-rust-library (concat (xdg-runtime-dir) "/parinfer-rust-emacs/lib/libparinfer_rust.so"))
  (parinfer-rust-check-before-enable nil))
;; (parinfer-rust-preferred-mode "indent")

(provide 'nagy-lisp)
;;; nagy-lisp.el ends here
