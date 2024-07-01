{
  pkgs ? import <nixpkgs> { },
  lib ? pkgs.lib,
  epkgs ? pkgs.emacs.pkgs,
  path ? ./.,
}:

let
  final =
    lib.mapAttrs'
      (name: value: {
        name = lib.removeSuffix ".el" name;
        # The package
        value = pkgs.nur.repos.nagy.lib.emacsMakeSingleFilePackage {
          src = (path + "/${name}");
          epkgs = epkgs.overrideScope (_self: _super: final);
        };
      })
      # The elisp files in `path`
      (
        lib.filterAttrs
          # Filter elisp files
          (name: value: value == "regular" && lib.hasSuffix ".el" name)
          # All files in `path`
          (builtins.readDir path)
      );
in
final
