;;; nagy-corfu.el --- Autocompletion -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "30.1") corfu)

(use-package corfu
  :defer t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-quit-at-boundary nil)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  :bind
  (:map corfu-map
        ("RET" . nil)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<key-chord> f j" . corfu-insert)))

(provide 'nagy-corfu)
;;; nagy-corfu.el ends here
