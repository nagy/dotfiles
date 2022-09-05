{ config, pkgs, ... }:

let
  customEmacsPackages = pkgs.emacsPackagesFor config.services.emacs.package;
  emacsAndPackages = customEmacsPackages.withPackages (epkgs:
    (with epkgs;
      with epkgs.melpaPackages; [
        vterm
        pdf-tools
        # org-pdftools
        pass
      ]));
in {
  environment.systemPackages = [
    emacsAndPackages
    (pkgs.mu.override { inherit (customEmacsPackages) emacs; })
  ];
}
