{

  # pre merge of https://github.com/NixOS/nixpkgs/pull/191459
  # because line-height is too high with newest version
  # https://protesilaos.com/codelog/2022-09-14-iosevka-comfy-1-0-0/
  inputs.nixpkgs-iosevka-comfy-040.url =
    "github:NixOS/nixpkgs?rev=bef209dc55a6b760b95ef7c08486a574fbd0cdd9";

  outputs = { self, nixpkgs-iosevka-comfy-040 }: rec {

    nixosModules = with builtins;
      with nixpkgs-iosevka-comfy-040.lib;
      let
        files = readDir ./modules;
        fileNames = attrNames files;
        nameFunc = filename: removeSuffix ".nix" filename;
        preAttrList = map (it: {
          name = nameFunc it;
          value = import (./modules + "/${it}");
        }) fileNames;
        modules = listToAttrs preAttrList;
      in modules // {
        # hack, to pass in the input argument
        fonts = import ./modules/fonts.nix nixpkgs-iosevka-comfy-040;
        converted-hmmpv = let dotlib = lib;
        in { pkgs, lib, config, ... }:
        with dotlib pkgs;
        (conv-hmmpv2nixos hmmodule-mpv) { inherit pkgs lib config; };
        converted-hmzathura = let dotlib = lib;
        in { pkgs, lib, config, ... }:
        with dotlib pkgs;
        (conv-hmzathura2nixos hmmodule-zathura) { inherit pkgs lib config; };
        converted-hmreadline = let dotlib = lib;
        in { pkgs, lib, config, ... }:
        with dotlib pkgs;
        (conv-hmreadline2nixos hmmodule-readline) { inherit pkgs lib config; };
      };

    lib = pkgs:
      (rec {
        evalhmmodule = module: pkgs:
          (import ("${pkgs.home-manager.src}" + "/modules") {
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
      } // (import ./lib pkgs));

  };
}
