{ pkgs, lib, ... }:

{

  environment.systemPackages = [
    pkgs.wabt
    pkgs.wasmtime
    pkgs.wasmer
    # pkgs.wasmedge
  ];

  boot.binfmt.emulatedSystems = [ "wasm32-wasi" ];

  # not fulfilled by above "wasm32-wasi"
  boot.binfmt.registrations.wat = {
    recognitionType = "extension";
    magicOrExtension = "wat";
    interpreter = lib.getExe pkgs.wasmtime;
  };

  programs.git = {
    config = {
      diff = {
        wasm = {
          textconv = "${pkgs.wabt}/bin/wasm2wat";
          binary = true;
        };
      };
    };
  };

}
