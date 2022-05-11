{
  description = "my dotfiles";

  outputs = { self }: {

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
          value = import (./modules + "/${it}" );
        }) fileNames;
        modules = listToAttrs preAttrList;
      in modules;

    lib = pkgs:
      ({
        hmmodule-mpv = import ./hmmodule-mpv.nix;
        hmmodule-firefox = import ./hmmodule-firefox.nix;
        hmmodule-zathura = import ./hmmodule-zathura.nix;
        hmmodule-alacritty-day = import ./hmmodule-alacritty-day.nix;
        hmmodule-alacritty-night = import ./hmmodule-alacritty-night.nix;
        hmmodule-readline = import ./hmmodule-readline.nix;

        conv-hmzathura2nixos = import ./conv-hmzathura2nixos.nix pkgs;
        conv-hmmpv2nixos = import ./conv-hmmpv2nixos.nix pkgs;
        conv-hmreadline2nixos = import ./conv-hmreadline2nixos.nix pkgs;

        pkg-ala-switchers = import ./pkg-ala-switchers.nix pkgs;
        pkg-journal-git-store = pkgs.writeScriptBin "journal-git-store"
          (builtins.readFile ./bin/journal-git-store);
      } // (import ./lib.nix pkgs));

  };
}
