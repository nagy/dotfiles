{
  description = "my dotfiles";

  outputs = { self }: {

    lib.module-common = import ./module-common.nix;
    lib.module-x86_64-linux = import ./module-x86_64-linux.nix;
    lib.hmmodule-mpv = import ./hmmodule-mpv.nix;
    lib.hmmodule-firefox = import ./module-firefox.nix;
    lib.hmmodule-zathura = import ./hmmodule-zathura.nix;
    lib.hmmodule-alacritty-day = import ./hmmodule-alacritty-day.nix;
    lib.hmmodule-alacritty-night = import ./hmmodule-alacritty-night.nix;

    lib.fetch-home-manager = import ./fetch-home-manager.nix;
    lib.fetch-emacs-overlay = import ./fetch-emacs-overlay.nix;

  };
}
