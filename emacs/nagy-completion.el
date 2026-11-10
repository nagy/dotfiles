;;; nagy-completion.el --- Completion config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") nagy-use-package)

;; NIX-EMACS-PACKAGE: corfu
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

;; NIX-EMACS-PACKAGE: company
(use-package company
  :demand t
  :diminish 'company-mode
  :bind
  ("C-M-s-¢" . company-mode)
  (:map company-active-map
        ("RET" . nil)
        ("<return>" . nil)
        ("C-l" . company-complete-selection)
        ("<key-chord> f j" . company-complete-selection))
  ;; (:map doom-leader-map
  ;;       ("M-c" . company-mode))
  ;; :config
  ;; (map! :map company-active-map
  ;;       "RET" nil
  ;;       "<return>" nil)
  :config
  (global-company-mode)
  ;; Remove `company-childframe-unless-just-one-frontend'
  (setopt company-frontends (remq 'company-childframe-unless-just-one-frontend company-frontends))
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  ;; (company-tooltip-align-annotations t)
  (company-tooltip-idle-delay 0.1)
  (company-async-redisplay-delay 0.1)
  (company-dabbrev-downcase nil)
  ;; Only search the current buffer for `company-dabbrev' (a backend that
  ;; suggests text your open buffers). This prevents Company from causing
  ;; lag once you have a lot of buffers open.
  (company-dabbrev-other-buffers nil))

(provide 'nagy-completion)
;;; nagy-completion.el ends here
