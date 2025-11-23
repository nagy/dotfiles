;;; luac-mode.el --- luac-mode -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;; NIX-EMACS-PACKAGE: lua-mode
(require 'lua-mode)

(defcustom luac-program "luac"
  "Name of the luac executable."
  :type 'string
  :group 'lua
  )

;;;###autoload
(define-derived-mode luac-mode special-mode "Luac"
  "Pretty print compiled lua files."
  (with-silent-modifications
    (call-process-region nil nil
                         luac-program
                         t
                         t
                         nil
                         "-p"           ; parse only
                         "-l"           ; list
                         "-"            ; stdin
                         ))
  (goto-char (point-min)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.luac\\'" . luac-mode))

;; ;;;###autoload
;; (add-to-list 'magic-mode-alist '("Lua" . luac-mode))

(provide 'luac-mode)
;;; luac-mode.el ends here
