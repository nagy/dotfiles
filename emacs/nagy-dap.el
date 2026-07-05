;;; nagy-dap.el --- Description -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

(require 'diminish)

;; NIX-EMACS-PACKAGE: dash
(require 'dash)
(require 'general)

;; NIX-EMACS-PACKAGE: anaphora
(require 'anaphora)

;; NIX-EMACS-PACKAGE: dape
(use-package dape
  :defer t
  :custom
  (dape-buffer-window-arrangement 'right))
  ;; :config
  ;; (repeat-mode 1)
  ;; :bind


(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               `(dlv
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-cwd-fn
                 port :autoport
                 :request "launch"
                 :mode "debug"
                 :type "go"
                 :program ".")))
                 ;; :args [""]

;; (evil-global-set-key 'normal (kbd "ð") dape-global-map)
;; (evil-define-key 'normal prog-mode-map (kbd "ð") dape-global-map)
;; (keymap-set prog-mode-map "M-RET" dape-global-map)
;; (keymap-global-set "H-1" #'dape)
;; (keymap-global-set "H-2" #'dape-breakpoint-toggle)
;; (keymap-global-set "H-3" #'dape-continue)
;; (keymap-global-set "H-4" #'dape-until)
;; (keymap-global-set "H-5" #'dape-restart)

(provide 'nagy-dap)
;;; nagy-dap.el ends here
