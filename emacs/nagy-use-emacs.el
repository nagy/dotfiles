;;; nagy-use-emacs.el --- My hy config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") general nagy-use-package)

(require 'nagy-use-package)

(use-package emacs
  :preface
  (defun nagy-emacs-set-lexical ()
    (interactive)
    (setq lexical-binding t))
  :demand t
  :bind
  ("H-M-e" . emacs-lisp-mode)
  :pretty 'emacs-lisp-mode
  ("if" . if) ("else" . else)
  ("when" . when) ("unless" . unless)
  ("let" . let)
  ("aif" . if)
  ("alet" . let)
  ("or" . or) ("and" . and)
  ("nil" . null)
  ("eval" . eval)
  ("defun" . def)
  ("cond" . "∃")
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
  ("awhen" . when)
  ("car" . "⒈")
  ("cadr" . "⒉")
  ("caddr" . "⒊")
  ("propertize" . "≔")
  ("zerop" . [?0 (Br . Bl) ??])
  ("buffer-string" . [?𝒃 (Br . Bl) ?𝒔])
  ("buffer-name" . [?𝒃 (Br . Bl) ?𝒏])
  ("start-process" . [?𝒔 (Br . Bl) ?𝒑])
  ("call-process" . [?𝒄 (Br . Bl) ?𝒑])
  ("condition-case" . [?𝒄 (Br . Bl) ?𝒄])
  ("cl-loop" . loop)
  ("cl-defgeneric" . "𝕘")
  ("cl-defmethod" . "𝕞")
  ("cl-assert" . assert)
  ("cl-first" . "⒈")
  ("cl-second" . "⒉")
  ("cl-third" . "⒊")
  ("cl-fourth" . "⒋")
  ("cl-fifth" . "⒌")
  ("cl-sixth" . "⒍")
  ("cl-seventh" . "⒎")
  ("cl-eighth" . "⒏")
  ("cl-ninth" . "⒐")
  ("cl-tenth" . "⒑")
  ("pcase" . "〣")                      ; same as :match
  ("pcase-lambda" . "ƛ")
  ("pcase-let" . "⤥")
  ("pcase-setq" . "⤈")
  ;; ("defreader" . "ℝ")
  ;;             :eval-when-compile "ℂ"
  ;; :config
  ;; (add-hook! emacs-lisp-mode
  ;;   ;; This adds the paragraph symbol as valid function character
  ;;   (set-char-table-range (syntax-table) ?§ '(2)))
  )

(provide 'nagy-use-emacs)
;;; nagy-use-emacs.el ends here
