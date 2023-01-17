{ pkgs, ... }:

let
  nsxivBigThumbs = pkgs.nsxiv.overrideAttrs (old: {
    postPatch = (old.postPatch or "") + ''
      # increase thumbnail sizes
      substituteInPlace config.def.h \
              --replace '96, 128, 160' '96, 128, 160, 320, 640'  \
              --replace 'THUMB_SIZE = 3' 'THUMB_SIZE = 5'  \
    '';
  });
in {

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    # Configure X11 window manager
    displayManager.startx.enable = true;
    # https://discourse.nixos.org/t/enable-vertical-sync-on-amd-gpu/12369/5
    deviceSection = ''Option "TearFree" "true"''; # For amdgpu.
    # keyboard
    layout = "mine";
    extraLayouts = {
      mine = {
        description = "my custom xkb layout";
        languages = [ "deu" ];
        symbolsFile = (pkgs.writeText "myinclude.conf" ''
          xkb_symbols "mine" {
             include "pc"
             include "de"

             key <LEFT> { [ Left , Left  , U21C7, U21E0 ]};
             key   <UP> { [ Up   , Up    , U21C8, U21E1 ]};
             key <RGHT> { [ Right, Right , U21C9, U21E2 ]};
             key <DOWN> { [ Down , Down  , U21CA, U21E3 ]};

             key <PAUS> {
                    type = "FOUR_LEVEL",
                    symbols[Group1] = [ Pause, U23E3, U23E2, U25EB ]
             };

             key <ESC>  { [ Escape, Escape, U25EB, U25A6 ] };
             key <END>  { [ End, End,   U2194, U2195  ] };
             key <MENU> { [ Menu, Menu, U2194, U2195  ] };
             key <RWIN> { [ Hyper_R, Hyper_R ] };
             key <INS>  { [ Multi_key, Multi_key, U232F, U223F ] };
             // modifier_map Mod3   { <MENU> };
             modifier_map Mod3   { <RWIN> };
             key <TAB>  { [ Tab,  ISO_Left_Tab,  U22A2,  U22A3 ] };
             // not used on current keyboard
             // key <CAPS> { [ Escape, Escape, 4, 5  ] };
             key <SPCE> { [space, space, circle, U25A1 ] };
             key <BKSP> { [ BackSpace, Delete, U2B20, U2B21 ] };
             key <AD03> { [  e, E  , EuroSign, U1F5CC ] };
             key <I148> { [ XF86Calculator, XF86Calculator, U219C, U219D] };
             key <I152> { [ XF86Explorer, XF86Explorer, UFE4D, UFE49 ] };
             key <CUT>  { [ XF86Cut, XF86Cut, U27E4, U27E5 ]  };
             // this overwrites dead_caron
             key <AC11>  { [adiaeresis, Adiaeresis, U2234, U2235 ] };
             key <COPY>  { [ XF86Copy, XF86Copy, U230A, U2308 ] };
             key <PAST>  { [ XF86Paste, XF86Paste, U230B, U2309 ] };
             key <FK01>  { [F1,  F1,  U2641, U22CC] };
             key <FK02>  { [F2,  F2,  U2609, U22CB] };
             key <FK04>  { [F4,  F4,  U2642, U2640] };
             key <FK05>  { [F5,  F5,  U25B1, U25B0] };
             key <FK06>  { [F6,  F6,  U25FA, U25F8] };
             key <FK07>  { [F7,  F7,  U25FF, U25F9] };
             key <FK11>  { [F11, F11, U203D, U2E18] };
             key <FK12>  { [F12, F12, U2A00, U29C9] };
          };
        '');
      };
    };
    # logFile = "/dev/null"; # the default
    # windowManager.exwm.enable = true;
    # videoDrivers = [ "amdgpu" ];
    excludePackages = [ pkgs.xterm ];
  };

  environment.extraOutputsToInstall =
    [ "dev" "bin" "info" "man" "devdoc" "out" "lib" ];

  services.udisks2.enable = true;

  users.users.user.extraGroups = [ "video" "render" ];

  environment.etc."X11/xinit/xinitrc".text = ''
    set -e
    xset r rate 260 40
    ${pkgs.xorg.xhost}/bin/xhost +
    xsetroot -cursor_name left_ptr # make default cursor not cross
    [[ -f /etc/X11/Xresources ]] && xrdb /etc/X11/Xresources
    ${pkgs.unclutter-xfixes}/bin/unclutter &
    if [[ "$(tty)" == /dev/tty1 ]]; then
      WM=emacs exec emacs
    fi
    ${pkgs.sxhkd}/bin/sxhkd &
    exec ${pkgs.bspwm}/bin/bspwm
  '';

  environment.etc."X11/Xresources".text = ''
    *.font: Iosevka Comfy:size=12
    *.background: #000000
    *.foreground: #ffffff
    Xcursor.size: 48
    Xcursor.theme: whiteglass
    Xft.dpi: 192   # for firefox
  '';

  environment.systemPackages = [
    pkgs.xorg.xcursorthemes
    pkgs.dmenu
    pkgs.scrot
    pkgs.playerctl
    nsxivBigThumbs
    # pkgs.gimp
    # pkgs.xorg.xmodmap
    pkgs.xclip

    pkgs.firefox
    pkgs.brave
    pkgs.tor-browser-bundle-bin
    pkgs.bspwm
    pkgs.sxhkd
    pkgs.pulsemixer
    pkgs.pulseaudio # for pactl
    pkgs.poppler_utils # pdf utils
    (pkgs.gnupg.override { guiSupport = false; })

    # for container
    pkgs.binutils
    pkgs.util-linux
  ] ++ [
    (pkgs.callPackage ../keyboard { }).flasher
    (pkgs.callPackage ../pkg-ala-switchers.nix {
      hmmodules = {
        day = import ../hmmodule-alacritty-night.nix false;
        night = import ../hmmodule-alacritty-night.nix true;
      };
    })
  ];
}
