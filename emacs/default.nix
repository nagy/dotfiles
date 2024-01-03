{ lib, pkgs, emacs }:

let
  makePackage = src:
    pkgs.nur.repos.nagy.lib.emacsMakeSingleFilePackage {
      inherit emacs src;
      epkgs = emacs.pkgs.overrideScope' (_self: _super: final);
      pname = lib.removeSuffix ".el" (builtins.baseNameOf src);
    };
  onlyNagyFiles = lib.filterAttrs
    (name: value: value == "regular" && lib.hasPrefix "nagy" name)
    (builtins.readDir ./.);
  final = lib.mapAttrs'
    (name: value: {
      name = lib.removeSuffix ".el" name;
      value = makePackage (./. + "/${name}");
    })
    onlyNagyFiles;
in
final
