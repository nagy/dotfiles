{
  pkgs ? import <nixpkgs> { },
  lib ? pkgs.lib,
}:

{
  modules = (
    (lib.packagesFromDirectoryRecursive {
      directory = ./modules;
      callPackage = (x: _a: import x);
    })
    // {
      all = {
        imports = lib.filesystem.listFilesRecursive ./modules;
      };
    }
  );
}
