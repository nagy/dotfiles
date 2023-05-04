{ lib, pkgs, emacs }:

let
  makePackage = { path, warnIsError ? true }:
    let
      name = lib.substring 44 999 path;
      destination = "/" + name;
    in pkgs.nur.repos.nagy.lib.emacsMakeSingleFilePackage {
      inherit emacs warnIsError;
      src = (pkgs.writeTextFile {
        inherit name destination;
        text = builtins.readFile path;
      }) + destination;
      pname = name;
      packageRequires =
        pkgs.nur.repos.nagy.lib.emacsParsePackageSet { inherit emacs path; };
    };
in {

  nagy-elpher = makePackage { path = ./nagy-elpher.el; };
  nagy-emacs = makePackage { path = ./nagy-emacs.el; };
  nagy-formats = makePackage { path = ./nagy-formats.el; };
  nagy-misc = makePackage { path = ./nagy-misc.el; };
  nagy-modus-themes = makePackage { path = ./nagy-modus-themes.el; };
  nagy-nlinum = makePackage {
    path = ./nagy-nlinum.el;
    warnIsError = false;
  };
  nagy-pcap-converter = makePackage { path = ./nagy-pcap-converter.el; };
  nagy-qrcode = makePackage { path = ./nagy-qrcode.el; };
  nagy-quirky-shell-command =
    makePackage { path = ./nagy-quirky-shell-command.el; };
  nagy-use-package = makePackage { path = ./nagy-use-package.el; };
}
