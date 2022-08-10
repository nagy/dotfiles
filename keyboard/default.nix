{ pkgs ? import <nixpkgs> { }, rightSideMaster ? true, ... }:

with pkgs.lib; rec {
  flashScript = pkgs.writeShellScriptBin "flashScript" ''
    ${pkgs.avrdude}/bin/avrdude -p atmega32u4 -c avr109 -P /dev/ttyACM0 -U flash:w:${firmware.hex}:i
  '';
  firmware = pkgs.stdenv.mkDerivation {
    name = "nagy-qmk-keyboard-hex-firmware";

    # may later be replaced with pkgs.qmk-udev-rules
    # no, because fetchSubmodules
    src = pkgs.fetchFromGitHub {
      owner = "qmk";
      repo = "qmk_firmware";
      rev = "0.16.9";
      sha256 = "sha256-gnQ/hehxPiYujakJWZynAJ7plJiDciAG3NAy0Xl18/A=";
      fetchSubmodules = true;
    };

    nativeBuildInputs = [ pkgs.qmk ];

    # this allows us to not need the .git folder
    SKIP_VERSION = "1";

    patchPhase = ''
      runHook prePatch
      ${optionalString rightSideMaster ''
        substituteInPlace \
            keyboards/handwired/dactyl_manuform/6x6/keymaps/default/config.h \
            --replace MASTER_LEFT MASTER_RIGHT
      ''}
      # echo '#define TAPPING_TERM 1000' >> keyboards/handwired/dactyl_manuform/6x6/keymaps/default/config.h
      echo '#define MOUSEKEY_MOVE_DELTA 4' >> keyboards/handwired/dactyl_manuform/6x6/keymaps/default/config.h
      cp ${./keymap.c} keyboards/handwired/dactyl_manuform/6x6/keymaps/default/keymap.c
      cp ${./rules.mk} keyboards/handwired/dactyl_manuform/6x6/rules.mk
      runHook postPatch
    '';

    buildPhase = ''
      runHook preBuild
      make --jobs=1 handwired/dactyl_manuform/6x6:default
      runHook postBuild
    '';

    outputs = [ "out" "hex" ];

    installPhase = ''
      runHook preInstall
      mkdir -p $out/share/qmk
      install -Dm444 handwired_dactyl_manuform_6x6_promicro_default.hex $hex
      ln -s $hex $out/share/qmk/handwired_dactyl_manuform_6x6_promicro_default.hex
      runHook postInstall
    '';
  };
}
