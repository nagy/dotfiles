{
  pkgs ? import <nixpkgs> { },
  lib ? pkgs.lib,
}:

{
  modules = (
    lib.packagesFromDirectoryRecursive {
      directory = ./modules;
      callPackage = (x: _a: import x);
    }
  );

  live = import ./nixos-tmpfs-root.nix {
    extraModules = [
      (
        { lib, pkgs, ... }:
        {
          # boot.kernelPackages = pkgs.linuxPackages_latest;
          documentation.enable = lib.mkForce false;
          documentation.nixos.enable = lib.mkForce false;

          # maybe this one can also be achived with `hardware.amdgpu.initrd.enable`.
          # boot.initrd.kernelModules = [ "amdgpu" ];
        }
      )
    ];
  };
  live-graphical = import ./nixos-tmpfs-root.nix { extraModules = [ ./modules/desktop.nix ]; };
}
