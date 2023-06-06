{ lib, pkgs, emacs, emacs-overlay }:

let
  makePackage = { src }:
    pkgs.nur.repos.nagy.lib.emacsMakeSingleFilePackage {
      inherit emacs src;
      pname = lib.substring 44 999 src;
      packageRequires = pkgs.nur.repos.nagy.lib.emacsParsePackageSet {
        inherit emacs src;
        parser = pkgs.callPackage "${emacs-overlay}/parse.nix" { };
      };
    };
in {
  nagy-evil = makePackage { src = ./nagy-evil.el; };
  nagy-corfu = makePackage { src = ./nagy-corfu.el; };
  nagy-magit = makePackage { src = ./nagy-magit.el; };
  nagy-elpher = makePackage { src = ./nagy-elpher.el; };
  nagy-emacs = makePackage { src = ./nagy-emacs.el; };
  nagy-formats = makePackage { src = ./nagy-formats.el; };
  nagy-misc = makePackage { src = ./nagy-misc.el; };
  nagy-modus-themes = makePackage { src = ./nagy-modus-themes.el; };
  nagy-nlinum = makePackage { src = ./nagy-nlinum.el; };
  nagy-pcap-converter = makePackage { src = ./nagy-pcap-converter.el; };
  nagy-qrcode = makePackage { src = ./nagy-qrcode.el; };
  nagy-quirky-shell-command =
    makePackage { src = ./nagy-quirky-shell-command.el; };
  nagy-use-package = makePackage { src = ./nagy-use-package.el; };
}
