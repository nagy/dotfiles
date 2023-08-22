{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, nur }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ nur.overlay ];
      };
      lib = pkgs.lib;
      evalhmmodule = module:
        (import "${pkgs.home-manager.src}/modules" {
          inherit pkgs;
          configuration = { ... }: {
            imports = [ module ];
            # simulate state version. needed for flake build.
            home.stateVersion = "23.05";
            home.username = "user";
            home.homeDirectory = "/home/user/";
          };
        }).config;
    in
    {
      nixosModules =
        let
          files = builtins.readDir ./modules;
          fileNames = lib.attrNames files;
          preAttrList = map
            (name: {
              name = lib.removeSuffix ".nix" name;
              value = import (./modules + "/${name}");
            })
            fileNames;
          modules = lib.listToAttrs preAttrList;
        in
        modules // {
          converted-hmmpv = (import ./conv-hmmpv2nixos.nix evalhmmodule)
            (import ./hmmodule-mpv.nix);
          converted-hmzathura = (import ./conv-hmzathura2nixos.nix evalhmmodule)
            (import ./hmmodule-zathura.nix);
          converted-hmreadline = (import ./conv-hmreadline2nixos.nix evalhmmodule)
            (import ./hmmodule-readline.nix);
        };
      packages.x86_64-linux.blocker =
        pkgs.nur.repos.nagy.lib.mkRustScript { file = ./bin/blocker.rs; };
      packages.x86_64-linux.emacs = pkgs.emacs29-gtk3.pkgs.withPackages (epkgs:
        pkgs.lib.attrValues (import ./emacs {
          inherit pkgs;
          inherit (pkgs) lib;
          inherit (epkgs) emacs;
        }));

      lib = {
        pkg-journal-file-store = pkgs.writeScriptBin "journal-file-store"
          (builtins.readFile ./bin/journal-file-store);
      } // (import ./lib { inherit pkgs; });

    };
}
