;;; nagy-help.el --- Help and lookup config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") general nagy-use-package)

(require 'general)

(declare-function preload-url "local")
(declare-function json-parse-file "nagy-emacs")

;; NIX-EMACS-PACKAGE: helpful
(use-package helpful
  :commands (helpful--navigate)
  :preface
  (defun helpful--all-the-buttons ()
    (car (save-excursion
           (goto-char (point-min))
           (cl-loop for i from 1 to 1000
                    for btn = (forward-button 1 nil nil t)
                    when btn
                    when (string= "Navigate to definition" (button-get btn 'help-echo))
                    collect btn))))
  (defun helpful-jump-to-definition ()
    (interactive)
    (when-let* ((btn (helpful--all-the-buttons)))
      (helpful--navigate btn)))
  :config
  (put 'helpful--bookmark-jump 'bookmark-handler-type "Helpful")
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
  (:map helpful-mode-map
        ([remap dired-jump] . helpful-jump-to-definition)))

;; NIX-EMACS-PACKAGE: inspector
(use-package inspector
  :custom
  (inspector-truncation-limit 10000)
  (inspector-slice-size 10000)
  :general
  (:states 'normal :keymaps 'inspector-mode-map
           "C-o" #'inspector-pop)
  :init
  (nagy-same-window "^\\*inspector\\*"))

;; NIX-EMACS-PACKAGE: wordnut
(use-package wordnut
  :defer t
  :init
  (nagy-same-window "^\\*WordNut\\*")
  :config
  ;; Disable header line
  (defun wordnut--headerline ()))

;; NIX-EMACS-PACKAGE: devdocs
(use-package devdocs
  :defer t
  :commands (nagy-devdocs-install)
  :custom
  (devdocs-use-mathjax nil)
  :config
  ;; replace
  (defun devdocs--available-docs ()
    (json-parse-file (preload-url "https://devdocs.io/docs.json")
                     :object-type 'alist))
  (defun nagy-devdocs-install (orig-fun &rest args)
    (cl-letf (((symbol-function 'url-insert-file-contents)
               (lambda (url)
                 (insert-file-contents (preload-url url)))))
      (apply orig-fun args)))
  (advice-add 'devdocs-install :around #'nagy-devdocs-install)
  ;; (advice-remove 'password-store--run #'nagy-devdocs-install)
  :init
  (nagy-same-window "^\\*devdocs\\*")
  :general
  (:states 'motion :keymaps 'devdocs-mode-map
           [remap evil-jump-backward] #'devdocs-go-back
           [remap evil-jump-forward] #'devdocs-go-forward))
;; (add-hook 'python-mode-hook
;;           (lambda () (setq-local devdocs-current-docs '("python~3.13"))))

(provide 'nagy-help)
;;; nagy-help.el ends here
