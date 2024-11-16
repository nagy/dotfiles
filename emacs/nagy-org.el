;;; nagy-org.el --- My org config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") org ascii-art-to-unicode org-superstar org-appear org-ref mermaid-mode pikchr-mode markdown-mode orgit howm general nagy-use-package)

(require 'general)

;; (require 'nagy-use-package)

(use-package org
  :commands (find-file-org)
  :custom
  (org-image-actual-width 400)
  (org-todo-keywords '((sequence "WAITING" "TODO" "DONE")
                       (sequence "TO-WATCH" "WATCHING" "DONE")))
  (org-confirm-babel-evaluate nil)
  (org-tags-column -77)
  (org-startup-indented nil)
  ;; (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-modules nil)
  (org-return-follows-link t)
  :hook
  (org-mode . visual-line-mode)
  :config
  (defun find-file-org ()
    (interactive)
    (find-file org-directory))
  :bind
  ("H-M-o" . org-mode)
  ("s-ø" . find-file-org)
  ("<XF86Copy>" . org-open-at-point-global)
  (:map org-mode-map
        ("H-j"   . org-next-visible-heading)
        ("H-k"   . org-previous-visible-heading)
        ("H-s-n" . org-narrow-to-subtree)
        ("H-w"   . org-ctrl-c-ctrl-c)
        ("H-h"   . org-info-find-node)
        ("s-."   . org-ctrl-c-ctrl-c)
        ("<key-chord> f j" . org-ctrl-c-ctrl-c)
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
           "RET" #'org-return
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
        ([remap nagy-kill-this-buffer] . org-edit-src-abort)
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
  ("classDiagram" . defclass)
  ("pie" . "◐")
  ("gantt" . "↴")
  :general
  (:states 'normal :keymaps 'mermaid-mode-map
           "ö" #'mermaid-compile-buffer))

(use-package markdown-mode
  :commands (markdown-mark-subtree nagy-markdown-delete-subtree)
  :config
  (defun nagy-markdown-delete-subtree ()
    (interactive)
    (markdown-mark-subtree)
    (delete-region (region-beginning) (region-end)))
  ;; (map! :map markdown-mode-map :n "r" #'markdown-cycle)

  ;; TODO needs to be put into modus themes hook
  (set-face-attribute 'markdown-header-face-1 nil :font "Et Bembo" :height 2.0 :inherit 'modus-themes-heading-1)
  (set-face-attribute 'markdown-header-face-2 nil :font "Et Bembo" :height 1.5 :inherit 'modus-themes-heading-2)
  (set-face-attribute 'markdown-header-face-3 nil :font "Et Bembo" :height 1.2 :inherit 'modus-themes-heading-3)
  :hook
  (markdown-mode . markdown-toggle-markup-hiding)
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

(use-package org-element
  :defer t
  :config
  (setq org-element-use-cache nil)
  (setq org-element-cache-persistent nil))

(use-package ol
  :commands (org-link-expand-abbrev browse-url-interactive-arg)
  :preface
  (defun browse-url--org-link (url &optional _new-window)
    (interactive (browse-url-interactive-arg "URL: "))
    (browse-url (org-link-expand-abbrev url)))
  :custom
  (org-link-frame-setup
   '((file . find-file)))
  (org-link-abbrev-alist
   '(("gh" . "https://github.com/")
     ("gl" . "https://gitlab.com/")
     ("crate" . "https://crates.io/crates/")
     ("pypi" . "https://pypi.org/project/")
     ("npm" . "https://www.npmjs.com/package/"))))

(with-eval-after-load 'thingatpt
  (push "gh:" thing-at-point-uri-schemes)
  (push "gl:" thing-at-point-uri-schemes)
  (push "npm:" thing-at-point-uri-schemes)
  (push "pypi:" thing-at-point-uri-schemes))

(with-eval-after-load 'browse-url
  (push '("\\`gh:" . browse-url--org-link) browse-url-default-handlers)
  (push '("\\`gl:" . browse-url--org-link) browse-url-default-handlers)
  (push '("\\`npm:" . browse-url--org-link) browse-url-default-handlers)
  (push '("\\`pypi:" . browse-url--org-link) browse-url-default-handlers))
(provide 'nagy-org)
;;; nagy-org.el ends here
