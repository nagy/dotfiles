{ config, pkgs, lib, ... }:

let
  customEmacsPackages = pkgs.emacsPackagesFor config.services.emacs.package;
  emacs = customEmacsPackages.emacs;
  emacsAndPackages = customEmacsPackages.withPackages (epkgs:
    (lib.attrValues (import ../emacs { inherit pkgs lib emacs; }))
    ++ (with epkgs; [
      (sotlisp.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "speed-of-thought-lisp";
          rev = "bf1c906cfbd111d6d0218aa7b5f8f3d635d89083";
          hash = "sha256-GsrIqnz+hPR1S0SkvduYp0rzmSHYkDDzFVWsLlgYEuM=";
        };
      })
      vterm
      pdf-tools
      org-pdftools
      elfeed
      triples
      bufler
      osm
      lispy
      lispyville

      visual-fill-column

      yaml-mode
      csv-mode

      org
      ascii-art-to-unicode
      rustic
      format-all

      # password and secrets
      pass
      password-store
      password-store-otp

      git-modes
      tokei
      dired-narrow
    ]));
in {
  environment.systemPackages =
    [ emacsAndPackages (pkgs.mu.override { inherit emacs; }) ];
}
