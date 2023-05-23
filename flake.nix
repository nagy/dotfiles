{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in {
      nixosModules = let
        files = builtins.readDir ./modules;
        fileNames = pkgs.lib.attrNames files;
        nameFunc = pkgs.lib.removeSuffix ".nix";
        preAttrList = map (it: {
          name = nameFunc it;
          value = import (./modules + "/${it}");
        }) fileNames;
        modules = pkgs.lib.listToAttrs preAttrList;
      in modules // {
        converted-hmmpv = self.lib.conv-hmmpv2nixos self.lib.hmmodule-mpv;
        converted-hmzathura =
          self.lib.conv-hmzathura2nixos self.lib.hmmodule-zathura;
        converted-hmreadline =
          self.lib.conv-hmreadline2nixos self.lib.hmmodule-readline;
      };

      lib = (rec {
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

        hmmodule-mpv = import ./hmmodule-mpv.nix;
        hmmodule-firefox = import ./hmmodule-firefox.nix;
        hmmodule-zathura = import ./hmmodule-zathura.nix;
        hmmodule-readline = import ./hmmodule-readline.nix;

        conv-hmzathura2nixos = import ./conv-hmzathura2nixos.nix evalhmmodule;
        conv-hmmpv2nixos = import ./conv-hmmpv2nixos.nix evalhmmodule;
        conv-hmreadline2nixos = import ./conv-hmreadline2nixos.nix evalhmmodule;

        pkg-journal-file-store = pkgs.writeScriptBin "journal-file-store"
          (builtins.readFile ./bin/journal-file-store);
      } // (import ./lib { inherit pkgs; }));

    };
}
