{
  inputs.nixpkgs.url = "nixpkgs/nixos-23.11";
  inputs.haumea.url = "github:nix-community/haumea?ref=v0.2.2";
  inputs.haumea.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, nur, haumea }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ nur.overlay ];
      };
      lib = pkgs.lib;
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
          hmconvert = pkgs.nur.repos.nagy.lib.modules.hmconvert;
          hmconfig = {
            imports = [
              ./hmmodule-mpv.nix
              ./hmmodule-zathura.nix
              ./hmmodule-readline.nix
            ];
            nix.nixPath = [ "haumea=${haumea}" ];
          };
        };
      packages.${pkgs.system} = {
        blocker =
          pkgs.nur.repos.nagy.lib.mkRustScript { file = ./bin/blocker.rs; };
        emacs = pkgs.emacs29-gtk3.pkgs.withPackages (epkgs:
          pkgs.lib.attrValues (import ./emacs {
            inherit pkgs;
            inherit (pkgs) lib;
            inherit (epkgs) emacs;
          }));
        keyboard-firmware = import ./keyboard { inherit pkgs; };
      };
      lib = {
        pkg-journal-file-store = pkgs.writeScriptBin "journal-file-store"
          (builtins.readFile ./bin/journal-file-store);
      } // (import ./lib { inherit pkgs; });

    };
}
