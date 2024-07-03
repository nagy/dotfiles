{ pkgs, ... }:

{
  imports = [ ./shortcommands.nix ];

  environment.systemPackages = [
    pkgs.zig
    pkgs.zls
  ];

  nagy.shortcommands = {
    z = [ "zig" ];
    zb = [
      "zig"
      "build"
    ];
    zbs = [
      "zig"
      "build"
      "-O"
      "ReleaseSmall"
    ];
    zbe = [
      "zig"
      "build-exe"
    ];
    zbes = [
      "zig"
      "build-exe"
      "-O"
      "ReleaseSmall"
    ];
  };

}
