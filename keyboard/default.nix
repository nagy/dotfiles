{
  pkgs ? import <nixpkgs> { },
  lib ? pkgs.lib,
  rightSideMaster ? true,
  mkQmkFirmware ? pkgs.nur.repos.nagy.lib.mkQmkFirmware,
}:

mkQmkFirmware {
  name = "nagy-keyboard-firmware";
  keyboard = "handwired/dactyl_manuform/6x6";

  patchPhase = ''
    runHook prePatch
    ${lib.optionalString rightSideMaster ''
      substituteInPlace \
        keyboards/$keyboard/keymaps/$keymap/config.h \
        --replace MASTER_LEFT MASTER_RIGHT
    ''}
    echo '#define MOUSEKEY_MOVE_DELTA 4' >> keyboards/$keyboard/keymaps/$keymap/config.h
    cp ${./keymap.c} keyboards/$keyboard/keymaps/$keymap/keymap.c
    cp ${./rules.mk} keyboards/$keyboard/rules.mk
    runHook postPatch
  '';
}
