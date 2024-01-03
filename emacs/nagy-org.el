;;; nagy-org.el --- My org config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") org ascii-art-to-unicode org-superstar org-appear org-ref mermaid-mode pikchr-mode markdown-mode orgit general nagy-use-package)

(require 'general)

(require 'nagy-use-package)

(use-package org
  :custom
  (org-image-actual-width 400)
  (org-todo-keywords '((sequence "WAITING" "TODO" "DONE")
                       (sequence "TO-WATCH" "WATCHING" "DONE")))
  (org-confirm-babel-evaluate nil)
  (org-tags-column -77)
  (org-startup-indented nil)
  ;; (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  :bind
  ("H-M-o" . org-mode)
  (:map org-mode-map
        ("H-j"   . org-next-visible-heading)
        ("H-k"   . org-previous-visible-heading)
        ("H-s-n" . org-narrow-to-subtree)
        ("H-w"   . org-ctrl-c-ctrl-c)
        ("H-h"   . org-info-find-node)
        ("s-."   . org-ctrl-c-ctrl-c)
        ("<key-chord> f j" . org-ctrl-c-ctrl-c)
        ("<key-chord> ü q" . org-shifttab)
        ("M-ŋ" . org-update-all-dblocks)
        ("C-H-#" . org-edit-special)
        ("A-s-j" . org-edit-special))
  :pretty 'org-mode
  ("#+begin_example" . "↝")
  ("#+end_example" . "↜")
  ("#+begin_src" . "»")
  ("#+end_src" . "«")
  ("#+BEGIN_SRC" . "»")
  ("#+END_SRC" . "«")
  ("#+begin_quote" . "“")
  ("#+end_quote" . "”")
  ("#+BEGIN_QUOTE" . "“")
  ("#+END_QUOTE" . "”")
  ("#+title:" . "⨠")
  ("#+TITLE:" . "⨠")
  ("#+name:" . "⁝")
  ("#+NAME:" . "⁝")
  ("#+date:" . "")
  ("#+DATE:" . "")
  ("#+begin_comment" . "󰿟")
  ("#+end_comment" . "󰿟")
  ("#+BEGIN_COMMENT" . "󰿟")
  ("#+END_COMMENT" . "󰿟")
  ("#+results:" . "‼")
  ("#+RESULTS:" . "‼")
  ("#+begin:" . "»")
  ("#+BEGIN:" . "»")
  ("#+end:" . "«")
  ("#+END:" . "«")
  ("[ ]" . "☐")
  ("[X]" . "☑")
  ("[-]" . "❍")
  (">=" . "≥")
  ("<=" . "≤")
  ("->" . "→")
  ("<-" . "←")
  ("<->" . "↔")
  ("=>" . "⇒")
  ("<=" . "⇐")
  :cycle 'org-mode
  ("no" "yes")
  ("output" "value")
  :general
  (:states 'normal :keymaps 'org-mode-map
           "ŋ" #'org-update-all-dblocks
           "r" #'org-cycle))

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :custom
  (org-superstar-leading-bullet ""))

(use-package org-appear
  :disabled
  :after org
  :custom
  (org-hide-emphasis-markers t)
  (org-appear-autolinks t)
  (org-appear-autokeywords t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t))

(use-package org-src
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

(require 'markdown-mode)
(use-package markdown-mode
  :preface
  (defun nagy-markdown-delete-subtree ()
    (interactive)
    (markdown-mark-subtree)
    (delete-region (region-beginning) (region-end)))
  :bind
  ("H-M-m" . markdown-mode)
  (:map markdown-mode-map
        ("H-d" . nagy-markdown-delete-subtree)
        ("H-j" . markdown-next-visible-heading)
        ("H-k" . markdown-previous-visible-heading)
        ("H-s-n" . markdown-narrow-to-subtree))
  :custom
  (markdown-list-indent-width 2))

(use-package org-agenda
  :defer t
  :config
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-files nil))

(provide 'nagy-org)
;;; nagy-org.el ends here
