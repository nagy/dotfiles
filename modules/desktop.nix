{ config, lib, pkgs, ... }:

let
  nsxivBigThumbs = pkgs.nsxiv.overrideAttrs (old: {
    postPatch = ''
      # increase thumbnail sizes
      substituteInPlace config.def.h \
              --replace '96, 128, 160' '96, 128, 160, 320, 640'  \
              --replace 'THUMB_SIZE = 3' 'THUMB_SIZE = 5'  \
    '';
  });
in {

  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    layout = "de";
    # make the accents key input the character with only one press instead of two
    xkbVariant = "nodeadkeys";
    # Configure X11 window manager
    displayManager.startx.enable = true;
    # logFile = "/dev/null"; # the default
    # windowManager.exwm.enable = true;
    videoDrivers = [ "amdgpu" ];
    excludePackages = [ pkgs.xterm ];
  };

  users.users.user.extraGroups = [ "video" "render" ];

  environment.systemPackages = [
    pkgs.xorg.xcursorthemes
    pkgs.dmenu
    pkgs.scrot
    pkgs.playerctl
    nsxivBigThumbs
  ];
}
