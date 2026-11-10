;;; nagy-documents.el --- Document and printing config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package reformatter)

(require 'general)

;; NIX-EMACS-PACKAGE: pdf-tools
(use-package pdf-tools
  :defer t
  :commands (pdf-tools-install-noverify)
  :custom
  (pdf-view-use-scaling nil)
  (pdf-view-midnight-colors '("white" . "black"))
  :bind
  (:map pdf-view-mode-map
        ;; ("H-j" . pdf-view-next-page-command)
        ;; ("H-k" . pdf-view-previous-page-command)
        ("M-m" . pdf-view-midnight-minor-mode))

  :hook
  (pdf-view-mode-hook . pdf-view-themed-minor-mode)
  (pdf-view-mode-hook . pdf-view-fit-page-to-window)
  :config
  (pdf-tools-install-noverify)
  ;; do not issue warning
  (setq pdf-view-incompatible-modes
        (delq 'display-line-numbers-mode pdf-view-incompatible-modes)))

(defun nagy/pdf-to-text (&optional arg)
  (interactive "P")
  (let ((buf (generate-new-buffer "*pdftext*")))
    (if arg
        (call-process "pdftotext" nil buf nil (buffer-file-name) "-")
      (call-process "pdftotext" nil buf nil "-layout" "-nopgbrk"
                    ;; "-f" (number-to-string (pdf-view-current-page))
                    ;; "-l" (number-to-string (pdf-view-current-page))
                    (buffer-file-name)
                    "-"))
    (switch-to-buffer buf)
    (text-mode)
    (goto-char (point-min))))

(use-package ps-print
  :defer t
  :custom
  (ps-print-header-frame nil))

;; TODO check why locale timestamp is not printed like this
(defun nagy-ps-time-stamp-dd.mm.yyyy ()
  "Return date as \"2001-06-18\" (ISO date)."
  (format-time-string "%d.%m.%Y"))

(declare-function image-mode-window-get "image-mode")
(declare-function ps-print-with-faces "ps-print")
(defun print-dwim ()
  (interactive)
  (require 'lpr)
  (let ((tmpfilename (make-temp-file "printed" nil ".pdf")))
    (pcase major-mode
      ('pdf-view-mode
       (if (buffer-file-name)
           (call-process lpr-command nil nil nil "-o"
                         (format "page-ranges=%d"
                                 (progn
                                   ;; this is just #'pdf-view-current-page inlined
                                   (image-mode-window-get 'page)))
                         (buffer-file-name))
         (user-error "No buffer file name for printing")))
      (_ (let ((file (make-temp-file "print-dwim" nil ".ps")))
           (if (region-active-p)
               (ps-print-region (region-beginning) (region-end) file)
             ;; (ps-print-buffer file)
             (ps-print-with-faces (point-min) (point-max) file))

           ;; nix-build "<nixpkgs>" -A ghostscript
           (call-process "/nix/store/fy54dwdr2wxf1phq3sgvrdyqdimzb48p-ghostscript-with-X-10.06.0/bin/ps2pdf" nil nil nil "-sPAPERSIZE=a4" file tmpfilename)
           (find-file tmpfilename))))))
(keymap-global-set "<print>" #'print-dwim)

;; NIX-EMACS-PACKAGE: typst-ts-mode
(use-package typst-ts-mode
  :preface
  (reformatter-define typstyle
    :group 'emacs
    :program "typstyle")
  :defer t
  :bind
  ("H-M-T" . typst-ts-mode)
  (:map typst-ts-mode-map
        ("C-⊢" . typstyle-buffer))
  :hook
  (typst-ts-mode-hook . typstyle-on-save-mode)
  :general
  (:states 'normal :keymaps 'typst-ts-mode-map
           "⊢" #'typstyle-buffer))

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

(provide 'nagy-documents)
;;; nagy-documents.el ends here
