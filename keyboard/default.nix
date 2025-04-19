{
  pkgs ? import <nixpkgs> { },
  mkQmkFirmware ? pkgs.nur.repos.nagy.lib.mkQmkFirmware,
}:

let
  keyboard = "handwired/dactyl_manuform/6x6";
  keymap = "nagymap";
  config_h = pkgs.writeText "config.h" ''
    #define MASTER_RIGHT
    #define MOUSEKEY_MOVE_DELTA 4
    #define DEBOUNCE 60
  '';
  rules_mk = pkgs.writeText "rules.mk" ''
    CONSOLE_ENABLE = no         # Console for debug
    NKRO_ENABLE = no            # Enable N-Key Rollover
    BACKLIGHT_ENABLE = no       # Enable keyboard backlight functionality
    RGBLIGHT_ENABLE = no        # Enable keyboard RGB underglow
    AUDIO_ENABLE = no           # Audio output
    DEBOUNCE_TYPE=sym_eager_pr  # maybe try out _pk if per row is not enough

    DEFAULT_FOLDER = handwired/dactyl_manuform/6x6/promicro
  '';
in
mkQmkFirmware {
  name = "nagy-keyboard-firmware";
  inherit keyboard keymap;

  patchPhase = ''
    runHook prePatch

    mkdir keyboards/${keyboard}/keymaps/${keymap}/
    cp ${config_h} keyboards/${keyboard}/keymaps/${keymap}/config.h
    cp ${./keymap.c} keyboards/${keyboard}/keymaps/${keymap}/keymap.c
    cp ${rules_mk} keyboards/${keyboard}/rules.mk

    runHook postPatch
  '';
  # debounce seemd to have helped a bit; but not enough, but very much better, but not perfect
  # https://github.com/qmk/qmk_firmware/blob/master/docs/feature_debounce_type.md
  # https://www.reddit.com/r/ErgoMechKeyboards/comments/rwd6ot/comment/hrbimdi/
}
