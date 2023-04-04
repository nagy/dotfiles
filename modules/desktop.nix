{ pkgs, config, ... }:

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

  imports = [
    ./converter.nix
  ];

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    # Configure X11 window manager
    displayManager.startx.enable = true;
    # https://discourse.nixos.org/t/enable-vertical-sync-on-amd-gpu/12369/5
    deviceSection = ''Option "TearFree" "true"''; # For amdgpu.
    # xkbDir = "${pkgs.xorg.xkeyboardconfig.overrideAttrs(old:{
    #   postPatch = "echo > rules/0026-evdev.m_s.part";
    # })}/etc/X11/xkb";
    # keyboard
    layout = "mine";
    extraLayouts = {
      mine = {
        description = "my custom xkb layout";
        languages = [ "deu" ];
        symbolsFile = pkgs.writeText "myinclude.conf" ''
          xkb_symbols "mine" {
             include "pc"
             include "de"
             include "inet(evdev)"

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
             // does not work yet because it gets overridden by inet(evdev)
             // see setxkbmap -print for why
             // key <KATA>  { [U2ADD, U22D4, U29FB, U223A] };
             key <HNGL>  { [U269B, U2607, U237E, U238D] };
             key <KATA>  { [U29FB, U29e7, includes, intersection] };
             key <HJCV>  { [U2A5A, U2A5B, U2A51, U2A52] };
             key <HENK>  { [U29D6, U27C1, U2220, U29A2] };
             key <HKTG>  { [U29C7, U29C8, U2A4f, U2A4E] };
             key <HIRA>  { [U1F5E4, U1F5E5, U223A, ff ] };
             // key <mineLower4> { [ ff, cent, U232F, Help ] };
             // same problem here
             key <FK13>  { [ff, ht, includes, intersection] };

             // modifier_map Mod3   { <MENU> };
             modifier_map Mod3   { <RWIN> };
             modifier_map Mod2   { <RCTL> };  // linux ALT key
             key <RCTL> { [ Alt_L, Alt_L ] };

             key <TAB>  { [ Tab,  ISO_Left_Tab,  U22A2,  U22A3 ] };
             // not used on current keyboard
             // key <CAPS> { [ Escape, Escape, 4, 5  ] };
             key <SPCE> { [space, space, circle, U25A1 ] };
             // this next has problems with meta prefix
             // key <BKSP> { [ BackSpace, Delete, U2B20, U2B21 ] };
             // this next is an attempt to fix that
             key <BKSP> {
                    type = "FOUR_LEVEL",
                    symbols[Group1] =  [ BackSpace, Delete, U2B20, U2B21 ]
             };
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
             // key <FK11>  { [F11, F11, U203D, U2E18] };
             key <FK12>  { [F12, F12, U2A00, U29C9] };
             // this is an attempt to get meta prefix to for for f-keys
             // This seems to have worked
             key <FK11> {
                    type = "FOUR_LEVEL",
                    symbols[Group1] = [F11, F11, U203D, U2E18]
             };

             // fix num block
             key <KP0>  { [KP_0, KP_0, KP_0, KP_0] };
             key <KP1>  { [KP_1, KP_1, KP_1, KP_1] };
             key <KP2>  { [KP_2, KP_2, KP_2, KP_2] };
             key <KP3>  { [KP_3, KP_3, KP_3, KP_3] };
             key <KP4>  { [KP_4, KP_4, KP_4, KP_4] };
             key <KP5>  { [KP_5, KP_5, KP_5, KP_5] };
             key <KP6>  { [KP_6, KP_6, KP_6, KP_6] };
             key <KP7>  { [KP_7, KP_7, KP_7, KP_7] };
             key <KP8>  { [KP_8, KP_8, KP_8, KP_8] };
             key <KP9>  { [KP_9, KP_9, KP_9, KP_9] };
          };
        '';
      };
    };
    # logFile = "/dev/null"; # the default
    # windowManager.exwm.enable = true;
    # videoDrivers = [ "amdgpu" ];
    excludePackages = [ pkgs.xterm ];
  };
  # for wayland compositors
  environment.variables.XKB_DEFAULT_LAYOUT = config.services.xserver.layout;

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
  '';

  environment.systemPackages = with pkgs; [
    # (pkgs.writeShellScriptBin "xkb-reeval" (let
    #   patchedSetXkbmap = pkgs.xorg.setxkbmap.overrideAttrs(old: {
    #     postInstall = (old.postInstall or "") + ''
    #        ln -sfn ${pkgs.xorg.xkeyboardconfig.overrideAttrs(old: { postPatch = "echo > rules/0026-evdev.m_s.part"; })}/etc/X11 $out/share/X11
    #     '';
    #   });
    #   in ''
    #     ${patchedSetXkbmap}/bin/setxkbmap -I/etc/X11/xkb -print | ${pkgs.xorg.xkbcomp}/bin/xkbcomp -I/etc/X11/xkb - $DISPLAY
    #   ''))
    xorg.xcursorthemes
    dmenu
    scrot
    playerctl
    nsxivBigThumbs
    # gimp
    # xorg.xmodmap
    xclip

    firefox
    brave
    tor-browser-bundle-bin
    bspwm
    sxhkd
    pulsemixer
    pulseaudio # for pactl
    poppler_utils # pdf utils
    (gnupg.override { guiSupport = false; })

    # for container
    binutils
    util-linux

    (callPackage ../pkg-ala-switchers.nix {
      hmmodules = {
        day = import ../hmmodule-alacritty-night.nix false;
        night = import ../hmmodule-alacritty-night.nix true;
      };
    })
  ];
}
