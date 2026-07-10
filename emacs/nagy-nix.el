;;; nagy-nix.el --- My nix config -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") reformatter nagy-use-package)

(require 'general)

;;;###autoload (autoload 'nixfmt-buffer "nagy-nix" nil t)
;;;###autoload (autoload 'nixfmt-region "nagy-nix" nil t)
;;;###autoload (autoload 'nixfmt-on-save-mode "nagy-nix" nil t)
(reformatter-define nixfmt
  :program "nixfmt"
  :args (list input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file-in-current-directory)
  :group 'nix)

;; NIX-EMACS-PACKAGE: nix-mode
(use-package nix-mode
  :custom
  (nix-repl-executable-args
   '("repl" "--expr" "with import <nixpkgs> {}; builtins // lib // pkgs // pkgs.nur.repos.nagy.lib"))
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
  ("H-M-n" . nix-mode))


;; NIX-EMACS-PACKAGE: nix-ts-mode
(use-package nix-ts-mode
  :defer t)
  ;; :config
  ;; (setq treesit-font-lock-level 4)
  ;; (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))


(define-auto-insert
  '("\\.nix\\'" . "Nix skeleton")
  '("Short description: "
    "{" \n
    "pkgs ? import <nixpkgs> { }," \n
    "# pkgs ? import (builtins.fetchTarball \"https://github.com/NixOS/nixpkgs/archive/pull/123456/head.tar.gz\") { }," \n
    "lib ? pkgs.lib," \n
    "stdenv ? pkgs.stdenv," \n
    "# fetchFromGitHub ? pkgs.fetchFromGitHub," \n
    "# writableTmpDirAsHomeHook ? pkgs.writableTmpDirAsHomeHook," \n
    "}:" \n
    \n
    "stdenv.mkDerivation (finalAttrs: {" \n
    "pname = \"\";" \n
    "version = \"\";" \n
    \n
    "src = fetchFromGitHub {" \n
    "owner = \"nagy\";" \n
    "repo = \"\";" \n
    "tag = \"\";" \n
    "# rev = \"\";" \n
    "# fetchSubmodules = true;" \n
    "hash = \"\";" \n
    "};" \n
    \n
    "# nativeBuildInputs = [ ];" \n
    "# buildInputs = [ ];" \n
    \n
    "# buildPhase = ''" \n
    "# runHook preBuild" \n
    "# runHook postBuild" \n
    "# '';" \n
    "# installPhase = ''" \n
    "# runHook preInstall" \n
    "# runHook postInstall" \n
    "# '';" \n

    "# preferLocalBuild = true;" \n
    "# allowSubstitutes = true;" \n

    "# meta = {" \n
    "# };" \n
    "})" \n
    \n))


(defun find-file-directory-nix ()
  (interactive)
  (cond
   ((file-exists-p "default.nix")
    (find-file "default.nix"))
   ((file-exists-p "flake.nix")
    (find-file "flake.nix"))
   (t (user-error "No Nix suitable file found"))))
(require 'dired)
(keymap-set dired-mode-map "H-M-n" #'find-file-directory-nix)

(use-package nix-prettify-mode
  :diminish nix-prettify-mode
  :custom
  (nix-prettify-char "┃▒"))
  ;; :hook
  ;; (dired-mode . nix-prettify-mode)
  ;; (nix-repl-mode . nix-prettify-mode)


;; TODO has an lsp nls
;; NIX-EMACS-PACKAGE: nickel-mode
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
