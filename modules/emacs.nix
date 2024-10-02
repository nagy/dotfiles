{
  config,
  pkgs,
  lib,
  nur,
  ...
}:

let
  emacs = pkgs.emacs30-gtk3;
  emacsPackages = pkgs.emacsPackagesFor emacs;
  customEmacsPackages = emacsPackages.overrideScope (
    self: super: {
      sotlisp = super.sotlisp.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "speed-of-thought-lisp";
          rev = "55eb75635490ec89c0903ccc21fd5c37fdb2a8d6";
          hash = "sha256-SZH4foUlazaJwlJAYGJNw2iTTvyQ6nrs1RhxppStILI=";
        };
      };
      memoize = super.memoize.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "emacs-memoize";
          rev = "33fcd1ec5a93f3768c43904fecc68399a84b8924";
          hash = "sha256-00C8WLR7CVCnp/VPgAP564XpMmXkaaddmi1tXdEevZI=";
        };
      };
      elisp-reader = nur.repos.nagy.emacsPackages.elisp-reader;
      obvious = nur.repos.nagy.emacsPackages.obvious;
    }
  );
  emacsAndPackages = customEmacsPackages.withPackages (
    epkgs: (lib.attrValues (import ../emacs { inherit pkgs lib epkgs; }))
  );
in
{
  environment.systemPackages = lib.mkIf config.services.xserver.enable [
    emacsAndPackages
    pkgs.mu
  ];

  # to allow "malloc-trim" to trim memory of emacs.
  boot.kernel.sysctl."kernel.yama.ptrace_scope" = lib.mkIf config.services.xserver.enable (
    lib.mkForce 0
  );
}
