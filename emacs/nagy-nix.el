;;; nagy-nix.el --- My nix config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Homepage: https://github.com/nagy/nagy
;; Package-Requires: ((emacs "29.1") nix-mode nickel-mode nagy-use-package)

(require 'eglot)

(require 'nagy-use-package)

(use-package nix-mode
  ;; :hook
  ;; (nix-mode . eglot-ensure)
  :pretty 'nix-mode
  ("true" . true) ("false" . false)
  ("if" . if) ("else" . else) ("then" . then)
  ("throw" . throw)
  ("self" . self)
  ("pkgs" . [?☐ (Bc . Bc) ?·])
  ("lib" . "☐")
  ("hash" . hash)
  ("let" . let)
  ("in" . in)
  ("src" . source)
  ("callPackage" . "𝕔")
  ("stdenv" . stdlib)
  ("description" . print)
  ("homepage" . "🌐")
  ("changelog" . "🍃")
  ("inherit" . "↧")
  ("import" . import)
  ("overrideAttrs" . "🌀")
  ;; ("mkDerivation" . [?○ (Bc . Bc) ?|])
  ("mkDerivation" . [?√ (Bc . Bc) ?|])
  ;; ("recurseIntoAttrs" . "➰")
  ("nameValuePair" . "⧉")
  ("builtins" . "𝕓")
  ("patches" . "🩹")
  ;; ("version" . "⪼")
  ("with" . [?↗ (Bl . Bl) ?↘])
  ("rec" . loop)
  ("meta" . meta)
  ("nativeBuildInputs" . "⬖")
  ("buildInputs" . "⬗")
  ("fetchurl" . "🧲")
  ("fetchFromGitHub" . [?🧲 (Br . Bl) ?′])
  ("fetchFromGitLab" . [?🧲 (Br . Bl) ?″])
  ("fetchFromSourcehut" . [?🧲 (Br . Bl) ?‴])
  ("fetchgit" . [?🧲 (Br . Bl) ?⁗])
  ;; Haumea
  ("root" . "√")
  ("python3" . "🐍")
  ("chicken" . "🐔")
  ;; Nixos modules
  ("boot" . "𝒃")
  ("config" . "𝒄")
  ("documentation" . "𝒅")
  ("environment" . "𝒆")
  ("hardware" . "𝒉")
  ("networking" . "𝒏")
  ("programs" . "𝒑")
  ("services" . "𝒔")
  ("time" . "𝒕")
  ("users" . "𝒖")
  ("imports" . "𝒊")
  ("options" . "𝒐")
  ("toString" . print)
  ("final" . "′")
  ("prev" . "″")
  ;; Nixos lib
  ("enable" . "¿")
  ("mkOption" . "⌥")
  ("mkIf" . if)
  ("mkEnableOption" . [?⌥ (Br . Bl) ?¿])
  ("nagy" . "ℕ")
  ("assert" . assert)
  ;; Flakes
  ("inputs" . import)
  ("outputs" . export)
  ("nixosModules" . "𝒎")
  ("packages" . "𝒑")
  ;; ("nixpkgs" . "📦")
  :abbrev 'nix-mode
  ("ts" . "toString")
  ;; ("ovr" . "overrideAttrs")
  ("orr" . "overrideAttrs")
  ("oo" . "override")
  ("ooo" . "overrideAttrs")
  ;; ("ov" . "override")
  ;; ("ova" . "overrideAttrs")
  ("rcl" . "runCommandLocal")
  ("ffgh" . "fetchFromGitHub")
  ("ffgl" . "fetchFromGitLab")
  ("ffsh" . "fetchFromSourcehut")
  ("nbi" . "nativeBuildInputs =")
  ("pbi" . "propagatedBuildInputs =")
  ("bi" . "buildInputs =")
  ("ih" . "inherit")
  ("pk" . "pkgs")
  ("li" . "lib")
  ("en" . "enable = true;")
  ("winx" . "with import <nixpkgs> { };")
  ("nvp" . "nameValuePair")
  :cycle 'nix-mode
  ("override" "overrideAttrs")
  ("fetchurl" "fetchFromGitHub" "fetchFromGitLab"  "fetchFromSourcehut")
  ("vendorHash" "vendorSha256")
  ("cargoHash" "cargoSha256")
  ("hash" "sha256")
  ("buildPhase" "installPhase")
  ("buildInputs" "nativeBuildInputs" "propagatedBuildInputs")
  ;; Hooks
  ("preInstall" "postInstall")
  ("preBuild" "postBuild")
  ("prePatch" "postPatch")
  ;; Flakes
  ("inputs" "outputs"))

(use-package nix-prettify-mode
  :custom
  (nix-prettify-char ?┃))

;; TODO has an lsp nls
(use-package nickel-mode
  :pretty 'nickel-mode
  ("let" . let)
  ("in" . in)
  ("if" . if) ("else" . else) ("then" . then))

(provide 'nagy-nix)
;;; nagy-nix.el ends here
