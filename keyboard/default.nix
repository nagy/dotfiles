{
  pkgs ? import <nixpkgs> { },
  mkQmkFirmware ? pkgs.nur.repos.nagy.lib.mkQmkFirmware,
}:

mkQmkFirmware {
  name = "nagy-keyboard-firmware";
  keyboard = "handwired/dactyl_manuform/6x6";
  # keymap = "default";

  patchPhase = ''
    runHook prePatch

    cp ${./config.h} keyboards/$keyboard/keymaps/$keymap/config.h
    cp ${./keymap.c} keyboards/$keyboard/keymaps/$keymap/keymap.c
    cp ${./rules.mk} keyboards/$keyboard/rules.mk

    runHook postPatch
  '';
  # debounce seemd to have helped a bit; but not enough, but very much better, but not perfect
  # https://github.com/qmk/qmk_firmware/blob/master/docs/feature_debounce_type.md
  # https://www.reddit.com/r/ErgoMechKeyboards/comments/rwd6ot/comment/hrbimdi/
}
