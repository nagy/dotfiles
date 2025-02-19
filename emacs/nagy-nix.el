;;; nagy-nix.el --- My nix config -*- lexical-binding: t; byte-compile-error-on-warn: t; -*-
;; Package-Requires: ((emacs "29.1") nix-mode nickel-mode reformatter general nagy-use-package)

(require 'general)

;;;###autoload (autoload 'nixfmt-buffer "nagy-nix")
;;;###autoload (autoload 'nixfmt-region "nagy-nix")
;;;###autoload (autoload 'nixfmt-on-save-mode "nagy-nix")
(reformatter-define nixfmt
  :program "nixfmt"
  :args (list input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file-in-current-directory)
  :group 'nix)

(use-package nix-mode
  :custom
  (nix-repl-executable-args
   '("repl" "--expr" "with import <nixpkgs> {}; builtins // lib // pkgs"))
  :defer t
  ;; :hook
  ;; (nix-mode . eglot-ensure)
  :hook
  (nix-mode . nixfmt-on-save-mode)
  :general
  (:states 'normal :keymaps 'nix-mode-map
           "⊢" #'nixfmt-buffer)
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
  ;; ("imports" . "𝒊")
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
  ("oo" . "override {}")
  ("ooo" . "overrideAttrs {}")
  ("ss" . "services")
  ("sd" . "systemd")
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
  ("libcs" . "lib.cleanSource")
  ("en" . "enable = true")
  ("inx" . "import <nixpkgs> { }")
  ("winx" . "with import <nixpkgs> { }")
  ("nvp" . "nameValuePair")
  :cycle 'nix-mode
  ("override" "overrideAttrs")
  ("fetchurl" "fetchFromGitHub" "fetchFromGitLab"  "fetchFromSourcehut")
  ("vendorHash" "vendorSha256")
  ("cargoHash" "cargoSha256")
  ("hash" "sha256")
  ("true" "false")
  ("buildPhase" "installPhase")
  ("buildInputs" "nativeBuildInputs" "propagatedBuildInputs")
  ;; Hooks
  ("preInstall" "postInstall")
  ("preBuild" "postBuild")
  ("prePatch" "postPatch")
  ;; Flakes
  ("inputs" "outputs")
  ;; :bind
  ;; (:map nix-mode-map
  ;;       ("C-⊢" . nixfmt-buffer))
  ;; :general
  ;; (:states 'normal :keymaps 'nix-mode-map
  ;;          "⊢" #'nixfmt-buffer)
  :bind
  ("H-M-n" . nix-mode)
  )

;; (use-package nix-prettify-mode
;;   :diminish 'nix-prettify-mode
;;   :custom
;;   (nix-prettify-char ?┃)
;;   :hook
;;   (dired-mode . nix-prettify-mode)
;;   (nix-repl-mode . nix-prettify-mode))

;; TODO has an lsp nls
(use-package nickel-mode
  :preface
  (reformatter-define nickel-format
    :group 'nickel
    :program "nickel"
    :args '("format"))
  :defer t
  :pretty 'nickel-mode
  ("let" . let)
  ("in" . in)
  ("if" . if) ("else" . else) ("then" . then)
  ("fun" . def)
  ("import" . import)
  :hook
  (nickel-mode . nickel-format-on-save-mode))

(provide 'nagy-nix)
;;; nagy-nix.el ends here
