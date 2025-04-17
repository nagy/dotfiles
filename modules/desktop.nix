{
  config,
  pkgs,
  lib,
  ...
}:

{
  # https://doc.qt.io/qt-6/highdpi.html#platform-details
  environment.sessionVariables.QT_USE_PHYSICAL_DPI = "1"; # for qt6
  # environment.sessionVariables.QT_SCALE_FACTOR = "2"; # for qutebrowser

  environment.extraOutputsToInstall = [
    "dev"
    "bin"
    "info"
    "man"
    "devdoc"
    "out"
    "lib"
  ];

  services.udisks2.enable = true;

  users.users.user.extraGroups = [
    "video"
    "render"
  ];

  environment.etc."X11/xinit/xinitrc".text = ''
    set -e
    xset r rate 260 40
    [[ -f /etc/X11/Xresources ]] && xrdb /etc/X11/Xresources
    ${pkgs.unclutter-xfixes}/bin/unclutter &
    exec emacs
  '';

  environment.systemPackages = lib.mkIf config.services.xserver.enable [
    (import ../pkg-ala-switchers.nix { inherit pkgs; })
  ];

  programs.gnupg = {
    # socket activation does not seem to be used. gnupg is starting an agent itself.
    # more info: https://discourse.nixos.org/t/how-to-make-gpg-use-the-agent-from-programs-gnupg-agent/11834/2

    # but also seems to be used. idk.
    # package = gnupg.override { guiSupport = false; };
    agent.enable = true;
    agent.settings = {
      default-cache-ttl = 34560000;
      max-cache-ttl = 34560000;
    };
  };
}
