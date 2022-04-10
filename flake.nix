{
  description = "my dotfiles";

  outputs = { self }: {

    nixosModules = {
      common = import ./module-common.nix;
      shortcommands = import ./module-shortcommands.nix;
      emacs = import ./module-emacs.nix;
    };

    lib = { pkgs, lib ? pkgs.lib }:
      ({
        module-common = import ./module-common.nix;
        module-shortcommands = import ./module-shortcommands.nix;
        module-emacs = import ./module-emacs.nix;
        module-x86_64-linux = import ./module-x86_64-linux.nix;
        hmmodule-mpv = import ./hmmodule-mpv.nix;
        hmmodule-firefox = import ./module-firefox.nix;
        hmmodule-zathura = import ./hmmodule-zathura.nix;
        hmmodule-alacritty-day = import ./hmmodule-alacritty-day.nix;
        hmmodule-alacritty-night = import ./hmmodule-alacritty-night.nix;
        hmmodule-readline = import ./hmmodule-readline.nix;

        fetch-home-manager = import ./fetch-home-manager.nix;
        fetch-emacs-overlay = import ./fetch-emacs-overlay.nix;

        conv-hmzathura2nixos = import ./conv-hmzathura2nixos.nix;
        conv-hmmpv2nixos = import ./conv-hmmpv2nixos.nix;
        conv-hmreadline2nixos = import ./conv-hmreadline2nixos.nix;

        pkg-ala-switchers = import ./pkg-ala-switchers.nix;
        pkg-journal-git-store = pkgs.writeScriptBin "journal-git-store"
          (builtins.readFile ./bin/journal-git-store);
      } // (import ./lib.nix { inherit pkgs; }));

  };
}
