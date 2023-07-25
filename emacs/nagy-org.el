;;; nagy-org.el --- My org config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") org ascii-art-to-unicode org-superstar org-appear org-ref mermaid-mode pikchr-mode general nagy-use-package)

(require 'general)

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

(use-package org-appear
  :disabled
  :after org
  :custom
  (org-hide-emphasis-markers t)
  (org-appear-autolinks t)
  (org-appear-autokeywords t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)
  :bind
  (:map org-src-mode-map
        ([remap save-kill-buffer] . org-edit-src-exit)
        ([remap kill-this-buffer] . org-edit-src-abort)
        ("H-s-e" . org-babel-tangle)))

(use-package ox-latex
  :defer t
  :config
  (add-to-list 'org-latex-classes
               '("dinbrief"
                 "\\documentclass{dinbrief}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package mermaid-mode
  :pretty 'mermaid-mode
  ("graph" . "⌥")
  ("sequenceDiagram" . "⏳")
  ("pie" . "◐")
  ("gantt" . "↴")
  :general
  (:states 'normal :keymaps 'mermaid-mode-map
           "ö" #'mermaid-compile-buffer))

(provide 'nagy-org)
;;; nagy-org.el ends here
