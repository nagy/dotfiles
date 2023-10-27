;;; nagy-hy.el --- My hy config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") hy-mode general nagy-use-package)

(require 'general)

(require 'nagy-use-package)

(use-package hy-mode
  :config
  (advice-add 'run-hy :around #'nagy-replace-switch-to-buffer-other-window)
  (setq hy-jedhy--enable? nil)
  ;; this does not work in :custom
  (setq hy-shell--interpreter-args nil)      ; remove --spy
  :bind
  ("H-M-P" . hy-mode)
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
  "^\\*Hy\\*$")

(provide 'nagy-hy)
;;; nagy-hy.el ends here
