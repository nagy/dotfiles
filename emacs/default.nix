{ lib, pkgs, emacs }:

let
  makePackage = src:
    pkgs.nur.repos.nagy.lib.emacsMakeSingleFilePackage {
      inherit emacs src;
      pname = lib.substring 44 999 src;
    };
in {
  nagy = makePackage ./nagy.el;
  nagy-evil = makePackage ./nagy-evil.el;
  nagy-corfu = makePackage ./nagy-corfu.el;
  nagy-company = makePackage ./nagy-company.el;
  nagy-common-lisp = makePackage ./nagy-common-lisp.el;
  nagy-magit = makePackage ./nagy-magit.el;
  nagy-elpher = makePackage ./nagy-elpher.el;
  nagy-emacs = makePackage ./nagy-emacs.el;
  nagy-formats = makePackage ./nagy-formats.el;
  nagy-forth = makePackage ./nagy-forth.el;
  nagy-misc = makePackage ./nagy-misc.el;
  nagy-modus-themes = makePackage ./nagy-modus-themes.el;
  nagy-nlinum = makePackage ./nagy-nlinum.el;
  nagy-pcap-converter = makePackage ./nagy-pcap-converter.el;
  nagy-python = makePackage ./nagy-python.el;
  nagy-text = makePackage ./nagy-text.el;
  nagy-qrcode = makePackage ./nagy-qrcode.el;
  nagy-quirky-shell-command = makePackage ./nagy-quirky-shell-command.el;
  nagy-use-package = makePackage ./nagy-use-package.el;
}
