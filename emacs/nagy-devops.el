;;; nagy-devops.el --- nagy-devops config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") groovy-mode terraform-mode gitlab-ci-mode dockerfile-mode jenkinsfile-mode cmake-mode nagy-use-package)

(require 'nagy-use-package)

(use-package groovy-mode
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
  ;; annotations
  ("@Memoized" . "🧠")
  ("@Lazy" . "💤")
  ("@CompileStatic" . "🧱")
  :abbrev 'groovy-mode
  ;; ("df" . "def")
  ("pr" . "println"))

(use-package jenkinsfile-mode
  :custom
  (jenkinsfile-mode-indent-offset 2))

(use-package cmake-mode)

(provide 'nagy-devops)
;;; nagy-devops.el ends here
