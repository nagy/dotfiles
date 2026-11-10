;;; nagy-search.el --- Search and edit-many config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: wgrep
(use-package wgrep
  :bind
  (:map wgrep-mode-map
        ([remap save-kill-buffer] . wgrep-finish-edit)
        ([remap kill-this-buffer] . wgrep-abort-changes)
        ([remap nagy-kill-this-buffer] . wgrep-abort-changes))

  :general
  (:states 'normal :keymaps 'grep-mode-map
           [remap evil-insert] #'wgrep-change-to-wgrep-mode)
  (:states 'normal :keymaps 'wgrep-mode-map
           "ö" #'wgrep-finish-edit))

;; NIX-EMACS-PACKAGE: iedit
(use-package iedit
  :bind
  ("C-;" . iedit-mode))

;; NIX-EMACS-PACKAGE: avy
(use-package avy
  :defer t
  :custom
  (avy-all-windows nil)
  (avy-background nil)
  ;; (avy-single-candidate-jump t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l
                 ?q ?w ?e ?r       ?i ?o ?p)))

;; NIX-EMACS-PACKAGE: expand-region
(use-package expand-region
  :bind
  ("M-→" . er/expand-region)
  ("M-←" . er/contract-region))

;; NIX-EMACS-PACKAGE: nhexl-mode
(use-package nhexl-mode
  :bind
  ("H-M-H" . nhexl-mode)
  ;; todo: upstream this into evil-collection
  :general
  (:states 'normal :keymaps 'nhexl-mode-map
           [remap evil-next-line] #'nhexl-next-line
           [remap evil-previous-line] #'nhexl-previous-line
           "0" #'nhexl-move-beginning-of-line
           "$" #'nhexl-move-end-of-line
           "w" #'nhexl-nibble-forward
           "b" #'nhexl-nibble-backward
           "gg" #'beginning-of-buffer
           "G" #'end-of-buffer))
;; (:states 'normal
;;          "⬡" #'nhexl-mode)

(provide 'nagy-search)
;;; nagy-search.el ends here
