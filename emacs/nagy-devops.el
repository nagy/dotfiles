;;; nagy-devops.el --- nagy-devops config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") groovy-mode terraform-mode hcl-mode gitlab-ci-mode dockerfile-mode jenkinsfile-mode cmake-mode nagy-use-package)

(require 'nagy-use-package)

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

(use-package jenkinsfile-mode
  :defer t
  :custom
  (jenkinsfile-mode-indent-offset 2)
  :pretty 'jenkinsfile-mode
  ("echo" . print))

(use-package gitlab-ci-mode
  :defer t
  :pretty 'gitlab-ci-mode
  ("variables" . let)
  ("default" . stdlib)
  ("artifacts" . [?𝒂 (Br . Bl) ?𝒓])
  ("script" . [?𝒔 (Br . Bl) ?𝒄])
  ("dependencies" . [?𝒅 (Br . Bl) ?𝒆]))

(use-package terraform-mode
  :defer t
  :pretty 'terraform-mode
  ("data" . [?𝒅 (Br . Bl) ?𝒂])
  ("provider" . [?𝒑 (Br . Bl) ?𝒓])
  ("resource" . [?𝒓 (Br . Bl) ?𝒆])
  ("output" . [?𝒐 (Br . Bl) ?𝒑])
  :config
  ;; (push '(terraform-mode "terraform-ls" "serve") eglot-server-programs)
  )

(provide 'nagy-devops)
;;; nagy-devops.el ends here
