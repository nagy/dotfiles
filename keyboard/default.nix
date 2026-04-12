{
  pkgs ? import <nixpkgs> { },
  mkQmkFirmware ? pkgs.nur.repos.nagy.lib.mkQmkFirmware,
}:

let
  keyboard = "handwired/dactyl_manuform/6x6";
  keymap = "default";
  config_h = pkgs.writeText "config.h" ''
    #define MASTER_RIGHT
    #define MOUSEKEY_MOVE_DELTA 4
    #define DEBOUNCE 60
    // This could be the solution to the key tapping problem.
    // Beginning with this firmware version or earlier, it seems that
    // a key, when pressed with a combination, is only registered once
    // the second key is released.
    // For example super-q is only registered when q is released and not when pressed.
    // maybe this could be the solution https://docs.qmk.fm/tap_hold#hold-on-other-key-press
    // this could be the changelog entry: https://docs.qmk.fm/ChangeLog/20230528#i-m-t-i
    // define HOLD_ON_OTHER_KEY_PRESS
    // It is still not okay...
    // then you hold key 1 and then press key 2 on another layer, then release key 1 then key two is registered with the new firmware, whereas key 1 would be have been registered with the 0.20.x version of the firmware
    // define PERMISSIVE_HOLD
  '';
  rules_mk = pkgs.writeText "rules.mk" ''
    BOOTMAGIC_ENABLE = no       # Enable Bootmagic Lite
    MOUSEKEY_ENABLE = yes       # Mouse keys
    EXTRAKEY_ENABLE = yes       # Audio control and System control
    CONSOLE_ENABLE = no         # Console for debug
    COMMAND_ENABLE = yes        # Commands for debug and configuration
    NKRO_ENABLE = no            # Enable N-Key Rollover
    BACKLIGHT_ENABLE = no       # Enable keyboard backlight functionality
    RGBLIGHT_ENABLE = no        # Enable keyboard RGB underglow
    AUDIO_ENABLE = no           # Audio output
    SPLIT_KEYBOARD = yes
    DEBOUNCE_TYPE=sym_eager_pr  # maybe try out _pk if per row is not enough

    DEFAULT_FOLDER = handwired/dactyl_manuform/6x6/promicro
  '';
in
mkQmkFirmware {
  name = "nagy-keyboard-firmware";
  inherit keyboard keymap;

  patchPhase = ''
    runHook prePatch

    mkdir -p keyboards/${keyboard}/keymaps/${keymap}/
    cp ${config_h} keyboards/${keyboard}/keymaps/${keymap}/config.h
    cp ${./keymap.c} keyboards/${keyboard}/keymaps/${keymap}/keymap.c
    cp ${rules_mk} keyboards/${keyboard}/rules.mk

    runHook postPatch
  '';
  # debounce seemd to have helped a bit; but not enough, but very much better, but not perfect
  # https://github.com/qmk/qmk_firmware/blob/master/docs/feature_debounce_type.md
  # https://www.reddit.com/r/ErgoMechKeyboards/comments/rwd6ot/comment/hrbimdi/
}
