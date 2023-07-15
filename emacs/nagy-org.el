;;; nagy-org.el --- My org config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") org org-superstar nagy-use-package)

(require 'nagy-use-package)

(use-package org
  :pretty 'org-mode
  ("#+begin_example" . "↝")
  ("#+end_example" . "↜")
  ("#+title:" . "⨠")
  ("#+TITLE:" . "⨠")
  ("[ ]" . "☐")
  ("[X]" . "☑")
  ("[-]" . "❍")
  (">=" . "≥")
  ("<=" . "≤")
  ("->" . "→")
  ("<-" . "←")
  ("<->" . "↔")
  ("=>" . "⇒")
  ("<=" . "⇐"))

(use-package org-superstar
  :disabled
  :hook
  (org-mode . org-superstar-mode))

(provide 'nagy-org)
;;; nagy-org.el ends here
