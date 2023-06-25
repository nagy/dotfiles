;;; nagy-devops.el --nagy-devops config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") groovy-mode terraform-mode gitlab-ci-mode dockerfile-mode jenkinsfile-mode  nagy-use-package)

(require 'nagy-use-package)

(use-package groovy-mode
  :pretty 'groovy-mode
  ("this" . self)
  ("if" . if)
  ("true" . true) ("false" . false))

(provide 'nagy-devops)
;;; nagy-devops.el ends here
