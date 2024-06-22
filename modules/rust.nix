{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.rustc
    pkgs.rustfmt
    pkgs.cargo
    pkgs.cargo-bloat
    pkgs.rust-analyzer
    pkgs.rust-script
    pkgs.cargo-modules

    # convenience
    pkgs.cargo-watch
    pkgs.cargo-info
    pkgs.cargo-sort

    # wasm
    # pkgs.rustc-wasm32
    pkgs.binaryen
    pkgs.cargo-component
  ];
}
