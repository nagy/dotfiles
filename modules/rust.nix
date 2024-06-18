{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.rustc
    pkgs.rustfmt
    pkgs.cargo
    pkgs.cargo-bloat
    pkgs.rust-analyzer
    pkgs.rust-script
    # pkgs.cargo-modules # build failure

    # convenience
    pkgs.cargo-watch
    pkgs.cargo-info
    pkgs.cargo-sort

    # wasm
    ## rustc-wasm
    pkgs.binaryen
  ];
}
