;;; nagy-use-emacs.el --- My hy config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") general nagy-use-package)

(require 'nagy-use-package)

(use-package emacs
  :demand t
  :bind
  ("H-M-e" . emacs-lisp-mode)
  :pretty 'emacs-lisp-mode
  ("if" . if) ("else" . else)
  ("when" . when) ("unless" . unless)
  ("aif" . if)
  ("alet" . let)
  ;; ("or" . or) ("and" . and)
  ("nil" . null)
  ("eval" . eval)
  ("defun" . def)
  ("list" . list)
  ("rx" . "𝕏")
  ("let-alist" . "⋱")
  ("interactive" . "𝒊")
  ("defvar" . "𝕍")
  ("defmacro" . "𝕄")
  ("length" . "≢")
  ("setf" . setf)
  ("setq" . setq)
  ("setq-local" . "⇣")
  ("setopt" . setf)
  ("use-package" . "𝕌")
  ("optional" . "◌")
  ("key" . "⬚")
  ("rest" . "⤑")
  ("it" . "✦")                          ; anaphoric
  ("car" . "⒈")
  ("cadr" . "⒉")
  ("caddr" . "⒊")
  ("cl-loop" . loop)
  ("cl-defgeneric" . "𝕘")
  ("cl-defmethod" . "𝕞")
  ("cl-assert" . assert)
  ;; ("defreader" . "ℝ")
  ;;             :pcase "〣"                 ; same as :match
  ;;             :pcase-lambda "ƛ" :pcase-let "⤥"
  ;;             :pcase-setq "⤈"
  ;;             :eval-when-compile "ℂ"
  ;; :config
  ;; (add-hook! emacs-lisp-mode
  ;;   ;; This adds the paragraph symbol as valid function character
  ;;   (set-char-table-range (syntax-table) ?§ '(2)))
  )

(provide 'nagy-use-emacs)
;;; nagy-use-emacs.el ends here
