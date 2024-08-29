{ pkgs, ... }:

{
  imports = [ ./shortcommands.nix ];

  environment.systemPackages = [
    pkgs.zig
    pkgs.zls
  ];

  nagy.shortcommands = {
    zb = [
      "zig"
      "build"
    ];
    zbs = [
      "zig"
      "build"
      "-Doptimize=ReleaseSmall"
    ];
    zbe = [
      "zig"
      "build-exe"
    ];
    zbes = [
      "zig"
      "build-exe"
      "-Doptimize=ReleaseSmall"
    ];
    zbr = [
      "zig"
      "build"
      "run"
    ];
    zbt = [
      "zig"
      "build"
      "test"
    ];
  };

}
