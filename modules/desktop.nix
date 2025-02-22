{
  config,
  pkgs,
  lib,
  nur,
  ...
}:

{
  services.xserver = {
    enable = true;
    dpi = 192;
    # Configure X11 window manager
    displayManager.startx.enable = true;
  };

  # https://doc.qt.io/qt-6/highdpi.html#platform-details
  environment.variables.QT_USE_PHYSICAL_DPI = "1"; # for qt6
  # environment.variables.QT_SCALE_FACTOR = "2"; # for qutebrowser

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
    xset s 300 300
    xset r rate 260 40
    ${pkgs.xorg.xhost}/bin/xhost +
    xsetroot -cursor_name left_ptr # make default cursor not cross
    [[ -f /etc/X11/Xresources ]] && xrdb /etc/X11/Xresources
    ${pkgs.unclutter-xfixes}/bin/unclutter &
    exec emacs
  '';

  environment.systemPackages = lib.mkIf (config.services.xserver.enable) (
    with pkgs;
    [
      xorg.xcursorthemes
      xorg.xwininfo
      scrot
      nur.repos.nagy.nsxivBigThumbs
      xclip
      (redshift.override { withGeolocation = false; })

      brave
      tor-browser
      pulsemixer
      poppler_utils # pdf utils

      # for container
      binutils
      util-linux

      pyright
      yt-dlp

      (pass.withExtensions (exts: [ exts.pass-otp ]))
      age
      passage

      (zbar.override {
        withXorg = false;
        enableVideo = false;
      })

      (callPackage ../pkg-ala-switchers.nix {
        hmmodules = {
          day = import ../hmmodule-alacritty-night.nix false;
          night = import ../hmmodule-alacritty-night.nix true;
        };
      })
      ffmpeg_7-full
      pandoc
    ]
  );

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

  services.pulseaudio = {
    enable = true;
  };

  services.pipewire = {
    enable = false;
  };

  programs.wireshark = {
    enable = true;
  };
}
