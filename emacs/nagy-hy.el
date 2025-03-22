;;; nagy-hy.el --- Hy config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1") hy-mode general nagy-use-package)

(require 'general)
;; (require 'nagy-use-package)
(declare-function nagy-replace-switch-to-buffer-other-window "nagy-use-package")

(use-package hy-shell
  :defer t
  :config
  (advice-add 'run-hy :around #'nagy-replace-switch-to-buffer-other-window))

(use-package hy-mode
  :config
  (setq hy-jedhy--enable? nil)
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
  :config
  ;; this does not work in :custom because it is a variable
  (setq hy-shell--interpreter-args nil)      ; remove --spy
  )

(provide 'nagy-hy)
;;; nagy-hy.el ends here
