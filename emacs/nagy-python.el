;;; nagy-python.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

(require 'general)

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
  :same "^\\*Python")

;; ;; NIX-EMACS-PACKAGE: python-black
;; (use-package python-black
;;   :custom
;;   (python-black-extra-args '("--line-length" "100"))
;;   :hook
;;   (python-mode . python-black-on-save-mode))

(provide 'nagy-python)
;;; nagy-python.el ends here
