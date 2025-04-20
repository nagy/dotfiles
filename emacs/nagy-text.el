;;; nagy-text.el --- My text config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") evil general nagy-use-package)

(require 'nagy-use-package)
(require 'general)

;; NIX-EMACS-PACKAGE: jinx
(use-package jinx
  :general
  (:states 'normal
           "×" #'jinx-mode))

;; NIX-EMACS-PACKAGE: wordnut
(use-package wordnut
  :defer t
  :same "^\\*WordNut\\*"
  :config
  ;; Disable header line
  (defun wordnut--headerline ()))

;; NIX-EMACS-PACKAGE: lorem-ipsum
(use-package lorem-ipsum
  :general
  (:states 'normal
           "C-🫧" #'lorem-ipsum-insert-sentences
           "🫧" #'lorem-ipsum-insert-paragraphs))

;; NIX-EMACS-PACKAGE: pandoc
(use-package pandoc
  :preface
  (defun nagy-text-to-plain ()
    (interactive)
    (pcase-exhaustive major-mode
      ('mhtml-mode
       (let ((buffer (generate-new-buffer (concat "Pandoc Plain: " (buffer-name)))))
         (call-process-region nil nil "pandoc" nil buffer nil
                              "--to=plain"
                              "--wrap=none"
                              "--from=html")
         (switch-to-buffer  buffer)
         (goto-char (point-min))
         (text-mode)
         ;; (olivetti-mode)
         ;; (let ((inhibit-message t))
         ;;   (olivetti-set-width 80))
         ))))
  ;; (evil-global-set-key 'motion (kbd "g H-M-t") #'nagy-text-to-plain)
  (defun nagy-text-to-org ()
    (interactive)
    (pcase-exhaustive major-mode
      ('mhtml-mode
       (let ((buffer (generate-new-buffer (concat "Pandoc Org: " (buffer-name)))))
         (call-process-region nil nil "pandoc" nil buffer nil
                              "--to=org"
                              "--wrap=none"
                              "--from=html")
         (switch-to-buffer  buffer)
         (goto-char (point-min))
         (org-mode)
         ))))
  ;; (evil-global-set-key 'motion (kbd "g H-M-o") #'nagy-text-to-org)
  )

;; TODO markdown disable toggle markup when pretty key is pressed

(provide 'nagy-text)
;;; nagy-text.el ends here
