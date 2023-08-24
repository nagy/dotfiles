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
          rev = "3e862df12361848be978d366fd4d9b74ac37b6bf";
          hash = "sha256-cbuNNQjS6AMDIYsv5TRMysd+0aY02GZBY2Ada9EQ7ZY=";
        };
      })
      vterm
      pdf-tools
      org-pdftools
      triples
      bufler

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
