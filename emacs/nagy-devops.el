;;; nagy-devops.el --- devops config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") dockerfile-mode reformatter general nagy-use-package)

(require 'general)

;; NIX-EMACS-PACKAGE: groovy-mode
(use-package groovy-mode
  :defer t
  :custom
  (groovy-indent-offset 2)
  :pretty 'groovy-mode
  ;; builtins
  ("true" . true) ("false" . false)
  ("this" . self)
  ("if" . if) ("else" . else)
  ("throw" . throw)
  ("import" . import)
  ("return" . return)
  ("try" . try) ("catch" . except)
  ("def" . def) ("class" . defclass)
  ;; methods
  ("println" . print)
  ("while" . loop)
  ("String" . tostring)
  ("Object" . object)
  ;; ("Map" . map)
  ("void" . null)
  ("new" . new)
  ("final" . const)
  ("private" . "󰂵")
  ("List" . list)
  :abbrev 'groovy-mode
  ;; ("df" . "def")
  ("pr" . "println"))

;; NIX-EMACS-PACKAGE: jenkinsfile-mode
(use-package jenkinsfile-mode
  :defer t
  :custom
  (jenkinsfile-mode-indent-offset 2)
  :pretty 'jenkinsfile-mode
  ("echo" . print))

;; NIX-EMACS-PACKAGE: gitlab-ci-mode
(use-package gitlab-ci-mode
  :defer t
  :pretty 'gitlab-ci-mode
  ("variables" . let)
  ("default" . stdlib)
  ("artifacts" . [?𝒂 (Br . Bl) ?𝒓])
  ("script" . [?𝒔 (Br . Bl) ?𝒄])
  ("dependencies" . [?𝒅 (Br . Bl) ?𝒆]))

;; NIX-EMACS-PACKAGE: terraform-mode
(use-package terraform-mode
  :preface
  (reformatter-define terraform-fmt
    :program "tofu"
    :args '("fmt" "-")
    ;; :lighter " TFmt"
    :group 'terraform-mode)
  :defer t
  :pretty 'terraform-mode
  ("data" . [?𝒅 (Br . Bl) ?𝒂])
  ("provider" . [?𝒑 (Br . Bl) ?𝒓])
  ("resource" . [?𝒓 (Br . Bl) ?𝒆])
  ("output" . [?𝒐 (Br . Bl) ?𝒑])
  :general
  (:states 'normal :keymaps 'terraform-mode-map
           "⊢" #'terraform-fmt-buffer)
  :hook
  (terraform-mode . terraform-fmt-on-save-mode)
  ;; :config
  ;; (push '(terraform-mode "terraform-ls" "serve") eglot-server-programs)
  )

(provide 'nagy-devops)
;;; nagy-devops.el ends here
