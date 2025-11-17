;;; nagy-org.el --- My org config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") ascii-art-to-unicode org-ref orgit howm nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: org
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
  ;; (org-cycle-hide-drawer-startup nil)
  :hook
  (org-mode . visual-line-mode)
  :config
  (defun find-file-org ()
    (interactive)
    (find-file (format "%s/TODOs.org" org-directory)))
  ;; (with-eval-after-load 'info
  ;;   (add-to-list 'Info-url-alist '("org" . "https://orgmode.org/manual/%e.html")))
  :bind
  ("H-M-o" . org-mode)
  ("C-ø" . org-store-link)
  ("M-ø" . org-insert-last-stored-link)
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
        ("A-s-j" . org-edit-special)
        ("M-j" . org-move-subtree-down)
        ("M-k" . org-move-subtree-up)
        ("H-." . org-timestamp)
        ("H-," . org-todo)
        ("H-q" . org-set-tags-command)
        ("H-E" . org-export-dispatch)
        ("C-M-l" . org-toggle-link-display)
        )
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
  ("#+begin_export" . "𝚇")
  ("#+end_export" . "𝚇")
  ("#+BEGIN_EXPORT" . "𝚇")
  ("#+END_EXPORT" . "𝚇")
  ("#+title:" . "⨠")
  ("#+TITLE:" . "⨠")
  ("#+name:" . "⁝")
  ("#+NAME:" . "⁝")
  ("#+date:" . "")
  ("#+DATE:" . "")
  ("#+author:" . "🧍")
  ("#+AUTHOR:" . "🧍")
  ("#+email:" . "📧")
  ("#+EMAIL:" . "📧")
  ("#+options:" . "⮾")
  ("#+OPTIONS:" . "⮾")
  ("#+language:" . "🌍")
  ("#+LANGUAGE:" . "🌍")
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

;; NIX-EMACS-PACKAGE: org-superstar
(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :custom
  (org-superstar-leading-bullet ""))

;; NIX-EMACS-PACKAGE: org-appear
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

;; (use-package ox
;;   :defer t
;;   :preface
;;   (declare-function nagy-replace-switch-to-buffer-other-window "nagy-use-package")
;;   :config
;;   ;; (advice-add 'org-export-to-buffer :around #'nagy-replace-switch-to-buffer-other-window)
;;   )

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

;; NIX-EMACS-PACKAGE: mermaid-mode
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

;; NIX-EMACS-PACKAGE: markdown-mode
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
  (add-to-list 'thing-at-point-uri-schemes "gh:")
  (add-to-list 'thing-at-point-uri-schemes "gl:")
  (add-to-list 'thing-at-point-uri-schemes "npm:")
  (add-to-list 'thing-at-point-uri-schemes "pypi:"))

(with-eval-after-load 'browse-url
  (add-to-list 'browse-url-default-handlers '("\\`gh:" . browse-url--org-link))
  (add-to-list 'browse-url-default-handlers '("\\`gl:" . browse-url--org-link))
  (add-to-list 'browse-url-default-handlers '("\\`npm:" . browse-url--org-link))
  (add-to-list 'browse-url-default-handlers '("\\`pypi:" . browse-url--org-link)))

(provide 'nagy-org)
;;; nagy-org.el ends here
