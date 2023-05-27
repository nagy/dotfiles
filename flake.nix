{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
      evalhmmodule = module:
        (import "${pkgs.home-manager.src}/modules" {
          inherit pkgs;
          configuration = { ... }: {
            imports = [ module ];
            # simulate state version. needed for flake build.
            home.stateVersion = "21.11";
            home.username = "user";
            home.homeDirectory = "/home/user/";
          };
        }).config;
    in {
      nixosModules = let
        files = builtins.readDir ./modules;
        fileNames = pkgs.lib.attrNames files;
        preAttrList = map (name: {
          name = pkgs.lib.removeSuffix ".nix" name;
          value = import (./modules + "/${name}");
        }) fileNames;
        modules = pkgs.lib.listToAttrs preAttrList;
      in modules // {
        converted-hmmpv = (import ./conv-hmmpv2nixos.nix evalhmmodule)
          (import ./hmmodule-mpv.nix);
        converted-hmzathura = (import ./conv-hmzathura2nixos.nix evalhmmodule)
          (import ./hmmodule-zathura.nix);
        converted-hmreadline = (import ./conv-hmreadline2nixos.nix evalhmmodule)
          (import ./hmmodule-readline.nix);
      };

      lib = {
        pkg-journal-file-store = pkgs.writeScriptBin "journal-file-store"
          (builtins.readFile ./bin/journal-file-store);
      } // (import ./lib { inherit pkgs; });

    };
}
