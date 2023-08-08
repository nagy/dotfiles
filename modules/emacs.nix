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
          rev = "1762cb2d4b72315c54a17716f0e4ca7dbf28745e";
          hash = "sha256-nkNZmkTyoRshaPTvCzwbARUB1LTI9ML3HNcZdP4MrIc=";
        };
      })
      vterm
      pdf-tools
      org-pdftools
      triples
      bufler
      osm

      visual-fill-column

      format-all

      # password and secrets
      pass
      password-store
      password-store-otp

      tokei
      dired-narrow
    ]));
in
{
  environment.systemPackages =
    [ emacsAndPackages (pkgs.mu.override { inherit emacs; }) ];
}
