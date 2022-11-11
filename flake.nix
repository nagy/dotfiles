{

  # pre merge of https://github.com/NixOS/nixpkgs/pull/191459
  # because line-height is too high with newest version
  # https://protesilaos.com/codelog/2022-09-14-iosevka-comfy-1-0-0/
  inputs.nixpkgs-iosevka-comfy-040.url =
    "github:NixOS/nixpkgs?rev=bef209dc55a6b760b95ef7c08486a574fbd0cdd9";

  outputs = { self, nixpkgs-iosevka-comfy-040 }: {

    nixosModules = with builtins;
      let
        # copied from nixpkgs lib.nix
        lib = {
          removeSuffix = suffix: str:
            let
              sufLen = stringLength suffix;
              sLen = stringLength str;
            in if sufLen <= sLen && suffix
            == substring (sLen - sufLen) sufLen str then
              substring 0 (sLen - sufLen) str
            else
              str;
        };
        files = readDir ./modules;
        fileNames = attrNames files;
        nameFunc = filename: lib.removeSuffix ".nix" filename;
        preAttrList = map (it: {
          name = nameFunc it;
          value = import (./modules + "/${it}");
        }) fileNames;
        modules = listToAttrs preAttrList;
      in modules // {
        # hack, to pass in the input argument
        fonts = import ./modules/fonts.nix nixpkgs-iosevka-comfy-040;
      };

    lib = { pkgs }:
      (rec {
        evalhmmodule = module:
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

        conv-hmzathura2nixos =
          import ./conv-hmzathura2nixos.nix { inherit pkgs evalhmmodule; };
        conv-hmmpv2nixos =
          import ./conv-hmmpv2nixos.nix { inherit pkgs evalhmmodule; };
        conv-hmreadline2nixos =
          import ./conv-hmreadline2nixos.nix { inherit pkgs evalhmmodule; };

        pkg-journal-file-store = pkgs.writeScriptBin "journal-file-store"
          (builtins.readFile ./bin/journal-file-store);
      } // (import ./lib { inherit pkgs; }));

  };
}
