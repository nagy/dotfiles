{ lib, pkgs, ... }:

{
  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/hardware/i2c.nix
  hardware.i2c.enable = true;
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "monbrightness" ''
      set -eux
      ${lib.getExe pkgs.ddcutil} --display=1 setvcp 10 "$1"
      ${lib.getExe pkgs.ddcutil} --display=2 setvcp 10 "$1"
    '')
  ];
}
