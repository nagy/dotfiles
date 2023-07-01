;;; nagy-nix.el --- My nix config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") nix-mode nagy-use-package)

(require 'nagy-use-package)

(use-package nix-mode
  :pretty 'nix-mode
  ("true" . true) ("false" . false)
  ("throw" . throw)
  ("self" . self)
  ;; ("pkgs" . "☐·")
  ;; ("pkgs" . "⧈")
  ("lib" . "☐")
  ("hash" . hash)
  ("let" . let)
  ("src" . source)
  ("callPackage" . "𝕔")
  ("stdenv" . stdlib)
  ("description" . print)
  ("homepage" . "🌐")
  ("inherit" . "↧")
  ("import" . import)
  ("overrideAttrs" . "🌀")
  ("nameValuePair" . "⧉")
  ("builtins" . "𝕓")
  ("patches" . "🩹")
  ;; ("with" . "⤭")
  ("rec" . loop)
  ("meta" . meta)
  ;; ("buildInputs" . "⬗")
  ;; ("nativeBuildInputs" . "⬖")
  ("fetchurl" . "🧲")
  ("fetchFromGitHub" . "🧲′")
  ("fetchFromGitLab" . "🧲″")
  ("fetchFromSourcehut" . "🧲‴")
  ;; Haumea
  ("root" . "√"))

(provide 'nagy-nix)
;;; nagy-nix.el ends here
