{ pkgs, ... }:

let
  # rustc-wasm = pkgs.rust-bin.stable.latest.default.override {
  #   extensions = [ "rust-src" ];
  #   targets = [ "wasm32-wasi" ];
  # };
in
{
  environment.systemPackages = [
    pkgs.rustc
    pkgs.rustfmt
    pkgs.cargo
    pkgs.cargo-deps
    pkgs.cargo-bloat
    pkgs.rust-analyzer
    pkgs.rust-script
    pkgs.cargo-modules

    # convenience
    pkgs.cargo-watch
    pkgs.cargo-info
    pkgs.cargo-sort

    # wasm
    ## rustc-wasm
    pkgs.binaryen
  ];
}
