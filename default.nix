{
  pkgs ? import <nixpkgs> { },
  lib ? pkgs.lib,
}:

let
  mkRustScript = pkgs.nur.repos.nagy.lib.mkRustScript;

  scriptFiles = lib.filter (f: lib.hasSuffix ".rs" f) (lib.filesystem.listFilesRecursive ./bin);

  scripts = lib.listToAttrs (
    map (f: {
      name = lib.removeSuffix ".rs" (lib.baseNameOf f);
      value = mkRustScript { file = f; };
    }) scriptFiles
  );

  # Aggregate of all script binaries. Reuses the individual derivations above,
  # so nothing is built twice.
  bin = pkgs.symlinkJoin {
    name = "bin";
    paths = lib.attrValues scripts;
  };
in
scripts // { inherit bin; }
