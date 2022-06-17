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
    layout = "mine";
    # Configure X11 window manager
    displayManager.startx.enable = true;
    extraLayouts = {
      mine = {
        description = "my custom xkb layout";
        languages = [ "deu" ];
        symbolsFile = (pkgs.writeText "myinclude.conf" ''
          xkb_symbols "mine" {
             include "pc"
             include "de"
             // key <MENU> { [  Hyper_R  ] };
             key <RWIN> { [  Hyper_R, Hyper_R, 4, 5  ] };
             key <INS> { [ Multi_key, Multi_key, U22CB, U22CC  ] };
             // modifier_map Mod3   { <MENU> };
             modifier_map Mod3   { <RWIN> };
             key <TAB>  { [ Tab,  ISO_Left_Tab,  U22A3,  U22A2 ] };
             // not used on current keyboard
             // key <CAPS> { [ Escape, Escape, 4, 5  ] };
             key <SPCE> { [space, space, circle, U232F ] };
             // this overwrites dead_caron
             key <AC11>  { [adiaeresis, Adiaeresis, U2234, U2235 ] };
             key <COPY>  { [ XF86Copy, XF86Copy, U230A, U2308 ] };
             key <PAST>  { [ XF86Paste, XF86Paste, U230B, U2309 ] };
             key <FK01>  { [F1, F1, U2641, 9] };
             key <FK02>  { [F2, F2, U2609, 6] };
          };
        '');
      };
    };
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
    pkgs.gimp
    pkgs.xorg.xmodmap
    pkgs.xclip
  ] ++ [
    ((import ../pkg-ala-switchers.nix pkgs) {
      hmmodules = {
        day = import ../hmmodule-alacritty-day.nix;
        night = import ../hmmodule-alacritty-night.nix;
      };
    })
  ];
}
