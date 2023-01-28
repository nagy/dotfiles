{ config, pkgs, ... }:

let
  customEmacsPackages = pkgs.emacsPackagesFor config.services.emacs.package;
  nagy = customEmacsPackages.callPackage ../emacs { };
  emacsAndPackages = customEmacsPackages.withPackages (epkgs:
    (with epkgs;
      with epkgs.melpaPackages; [
        nagy.nagy-modus-themes
        nagy.nagy-nlinum
        nagy.nagy-formats
        nagy.nagy-elpher


        vterm
        # pdf-tools
        # org-pdftools
        elfeed
        pass
      ]));
in {
  environment.systemPackages = [
    emacsAndPackages
    (pkgs.mu.override { inherit (customEmacsPackages) emacs; })
  ];
}
