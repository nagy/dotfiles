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
      in modules // (with self.lib; {
        converted-hmmpv = conv-hmmpv2nixos hmmodule-mpv;
        converted-hmzathura = conv-hmzathura2nixos hmmodule-zathura;
        converted-hmreadline = conv-hmreadline2nixos hmmodule-readline;
      });

      lib = rec {
        hmmodule-mpv = import ./hmmodule-mpv.nix;
        hmmodule-firefox = import ./hmmodule-firefox.nix;
        hmmodule-zathura = import ./hmmodule-zathura.nix;
        hmmodule-readline = import ./hmmodule-readline.nix;

        conv-hmzathura2nixos = import ./conv-hmzathura2nixos.nix evalhmmodule;
        conv-hmmpv2nixos = import ./conv-hmmpv2nixos.nix evalhmmodule;
        conv-hmreadline2nixos = import ./conv-hmreadline2nixos.nix evalhmmodule;

        pkg-journal-file-store = pkgs.writeScriptBin "journal-file-store"
          (builtins.readFile ./bin/journal-file-store);
      } // (import ./lib { inherit pkgs; });

    };
}
