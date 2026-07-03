;;; nagy-typst.el --- My typst config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1")  nagy-use-package)

;; * Typst

;; NIX-EMACS-PACKAGE: typst-ts-mode
(use-package typst-ts-mode
  :preface
  (reformatter-define typstyle
    :group 'emacs
    :program "typstyle"
    )
  :defer t
  :bind
  ("H-M-T" . typst-ts-mode)
  (:map typst-ts-mode-map
        ("C-⊢" . typstyle-buffer))
  :hook
  (typst-ts-mode . typstyle-on-save-mode)
  :general
  (:states 'normal :keymaps 'typst-ts-mode-map
           "⊢" #'typstyle-buffer)
  )

;;  TODO integrate tinymist language server lsp https://github.com/Myriad-Dreamin/tinymist

;; NIX-EMACS-PACKAGE: ox-typst
(use-package ox-typst
  :commands (org-typst-export-to-typst) ;; for autoload
  :after org
  :custom
  (org-typst-export-buffer-major-mode 'typst-ts-mode)
  ;; :config
  ;; (add-to-list 'org-export-options-alist
  ;;              '(:with-phone nil "phone" nil t))
  :bind
  (:map org-mode-map
        ("H-M-T" . org-typst-export-as-typst)
        ("H-M-P" . org-typst-export-to-pdf)))


(provide 'nagy-typst)
;;; nagy-typst.el ends here
