{
  config,
  pkgs,
  lib,
  nur,
  ...
}:

let
  emacs = pkgs.emacs-git.override { withGTK3 = true; };
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
    }
  );
  emacsAndPackages = customEmacsPackages.withPackages (
    epkgs: (lib.attrValues (import ../emacs { inherit pkgs lib epkgs; }))
  );
in
{
  nixpkgs.overlays = [
    # Cannot use pkgs.fetchFromGitHub in an overlay
    (import (fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/e237f6ef7ddd6d76c9e52125f88620a9051e85db.tar.gz";
      sha256 = "sha256-EFomVakrZ06L1RjA3Ux2Rf4uMSSSr6+Cvvlj7OhGxGA=";
    }))
  ];
  environment.systemPackages = lib.mkIf config.services.xserver.enable [
    emacsAndPackages
    pkgs.mu
  ];

  # to allow "malloc-trim" to trim memory of emacs.
  boot.kernel.sysctl."kernel.yama.ptrace_scope" = lib.mkIf config.services.xserver.enable (
    lib.mkForce 0
  );

  # try to not overcommit memory
  # [[info:emacs#Memory Full]]
  # https://www.gnu.org/software/emacs/manual/html_node/emacs/Memory-Full.html
  # boot.kernel.sysctl."vm.overcommit_memory" = lib.mkForce 2;
  # boot.kernel.sysctl."vm.overcommit_ratio" = lib.mkForce 0;
  # this may lead to "failed to start mount-pstore" systemd error
}
