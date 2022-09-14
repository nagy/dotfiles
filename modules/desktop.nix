{ pkgs, ... }:

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

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    layout = "mine";
    # Configure X11 window manager
    displayManager.startx.enable = true;
    # https://discourse.nixos.org/t/enable-vertical-sync-on-amd-gpu/12369/5
    deviceSection = ''Option "TearFree" "true"''; # For amdgpu.
    extraLayouts = {
      mine = {
        description = "my custom xkb layout";
        languages = [ "deu" ];
        symbolsFile = (pkgs.writeText "myinclude.conf" ''
          xkb_symbols "mine" {
             include "pc"
             include "de"

             key   <UP> { [ Up   , Up    , U21C8 ]};
             key <LEFT> { [ Left , Left  , U21C7 ]};
             key <DOWN> { [ Down , Down  , U21CA ]};
             key <RGHT> { [ Right, Right , U21C9 ]};

             key <ESC>  { [ Escape, Escape, U25EB, U25A6 ] };
             key <END>  { [ End, End,   U2194, U2195  ] };
             key <MENU> { [ Menu, Menu, U2194, U2195  ] };
             key <RWIN> { [  Hyper_R, Hyper_R ] };
             key <INS> { [ Multi_key, Multi_key, U232F, U223F ] };
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
    (import ../keyboard { inherit pkgs; }).flasher
    ((import ../pkg-ala-switchers.nix pkgs) {
      hmmodules = {
        day = import ../hmmodule-alacritty-day.nix;
        night = import ../hmmodule-alacritty-night.nix;
      };
    })
  ];
}
