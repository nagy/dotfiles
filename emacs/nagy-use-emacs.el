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
  ("let*" . "⇘")
  ("or" . or) ("and" . and)
  ("nil" . null)
  ("eval" . eval)
  ("defun" . def)
  ("cl-defun" . def)
  ("cond" . "∃")
  ("list" . list)
  ("t" . "𝒕")
  ("not" . not)
  ("rx" . "𝕏")
  ("let-alist" . "⋱")
  ("interactive" . "𝒊")
  ("defvar" . "𝕍")
  ("defmacro" . "𝕄")
  ("defclass" . defclass)
  ("length" . "≢")
  ("setf" . setf)
  ("setq" . setq)
  ("setq-local" . "⇣")
  ("setopt" . setf)
  ("progn" . "⋮")
  ("use-package" . "𝕌")
  ("&optional" . "◌")
  ("&key" . "⬚")
  ("&rest" . "⤑")
  ("it" . "✦")                          ; anaphoric
  ("awhen" . when)
  ("car" . "⒈")
  ("cadr" . "⒉")
  ("caddr" . "⒊")
  ("propertize" . "󰁦")
  ("zerop" . [?𝟎 (Br . Bl) ??])
  ("alist-get" . [?𝒂 (Br . Bl) ?𝒈])
  ("plist-get" . [?𝒑 (Br . Bl) ?𝒈])
  ("buffer-string" . [?𝒃 (Br . Bl) ?𝒔])
  ("eval-when-compile" . [?𝒆 (Br . Bl) ?𝒄])
  ("buffer-name" . [?𝒃 (Br . Bl) ?𝒏])
  ("erase-buffer" . [?𝒆 (Br . Bl) ?𝒃])
  ("save-excursion" . [?𝒔 (Br . Bl) ?𝒙])
  ("start-process" . [?𝒔 (Br . Bl) ?𝒑])
  ("call-process" . [?𝒄 (Br . Bl) ?𝒑])
  ("call-process-region" . [?𝒄 (Br . Bl) ?𝒓])
  ("condition-case" . [?𝒄 (Br . Bl) ?𝒄])
  ("default-directory" . [?𝒅 (Br . Bl) ?𝒅])
  ("string=" . [?𝒔 (Br . Bl) ?=])
  ("string-equal" . [?𝒔 (Br . Bl) ?=])
  ("with-environment-variables" . [?𝒘 (Br . Bl) ?𝒆])
  ("with-current-buffer" . [?𝒘 (Br . Bl) ?𝒄])
  ("with-temp-buffer" . [?𝒘 (Br . Bl) ?𝒕])
  ("string-prefix-p" . [? (Br . Bl) ??])
  ("string-suffix-p" . [? (Br . Bl) ??])
  ("thread-first" . [?| (Br . Bl) ?…])
  ("thread-last" . [?… (Br . Bl) ?|])
  ("concat" . "◇")
  ("string-remove-prefix" . [?𝒙 (Br . Bl) ?])
  ("string-remove-suffix" . [?𝒙 (Br . Bl) ?])
  ("advice-add" . [?𝒂 (Br . Bl) ?+])
  ("advice-remove" . [?𝒂 (Br . Bl) ?-])
  ("file-exists-p" . [?𝒇 (Br . Bl) ??])
  ("find-file" . [?𝒇 (Br . Bl) ?𝒇])
  ("set-face-attribute" . [?𝑭 (Br . Bl) ?=])
  ("switch-to-buffer" . [ ;; ?𝒔 (Br . Bl)
                            ?𝟐 (Br . Bl) ?𝒃])
  ("append" .  [?⋯ (Br . Bl) ?+])
  ("cl-loop" . loop)
  ("cl-defstruct" . "𝕤")
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
  ("cl-incf" .  [?+ (Br . Bl) ?=])
  ("cl-decf" .  [?- (Br . Bl) ?=])
  ("cl-callf" .  [?\( (Br . Bl) ?\)])
  ("require" . import)
  ("prog1" . "′")
  ("aprog1" . "′")
  ("prog2" . "″")
  ("aprog2" . "″")
  ("pcase" . "〣")                      ; same as :match
  ("pcase-lambda" . [?〣 (Br . Bl) ?λ])
  ("pcase-let" . [?〣 (Br . Bl) ?↘])
  ("pcase-setq" . [?〣 (Br . Bl) ?↡])
  ("pcase-dolist" . [?〣 (Br . Bl) ?↻])
  ("pcase-defmacro" . [?〣 (Br . Bl) ?𝕄])
  ("bindat-pack" . [?󱃲 (Br . Bl) ?↑])
  ("bindat-unpack" . [?󱃲 (Br . Bl) ?↓])
  ("bindat-type" . [?󱃲 (Br . Bl) ?𝒕])
  ("bindat-defmacro" . [?󱃲 (Br . Bl) ?𝒎])
  ("apcase" . "〣")                     ; anaphoric
  ;; dash.el
  ("-lambda" . "λ")
  ("thing" . "💎")
  :cycle 'emacs-lisp-mode
  ("nil" "t")
  ;; ("defreader" . "ℝ")
  ;;             :eval-when-compile "ℂ"
  ;; :config
  ;; (add-hook! emacs-lisp-mode
  ;;   ;; This adds the paragraph symbol as valid function character
  ;;   (set-char-table-range (syntax-table) ?§ '(2)))
  )

;; (use-package make-mode
;;   :pretty 'makefile-gmake-mode
;;   ("ifeq" . if) ("else" . else) ("endif" . else))

(provide 'nagy-use-emacs)
;;; nagy-use-emacs.el ends here
