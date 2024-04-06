{ lib, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    nixfmt
    nil
    nickel
    nix-prefetch
    nix-prefetch-git
    nix-diff
    nix-init
    nix-update
  ];

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes recursive-nix impure-derivations ca-derivations
    '';
    settings = {
      sandbox = true;
      auto-optimise-store = true;
      trusted-users = [
        "root"
        "@wheel"
      ];
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
      warn-dirty = false;
      eval-cache = false;
      # Build logs are backed up. Backup is supposed to already be compressed.
      compress-build-log = false;
      # this reduces memory usage at the expense of performance
      cores = 1;
      # this keeps build logs clean at the expense of performance
      max-jobs = 1;
    };
    nixPath = lib.mkOptionDefault [ "dot=${../.}" ];
    registry = {
      nagy.to = {
        owner = "nagy";
        repo = "nur-packages";
        type = "github";
      };
      N.to = {
        id = "nagy";
        type = "indirect";
      };
      dot.to = {
        owner = "nagy";
        repo = "dotfiles";
        type = "github";
      };

      lib.to = {
        owner = "NixOS";
        repo = "nixpkgs";
        type = "github";
        dir = "lib";
      };
      pkgs.to = {
        id = "nixpkgs";
        type = "indirect";
      };
      p.to = {
        id = "nixpkgs";
        type = "indirect";
      };
      u.to = {
        owner = "NixOS";
        repo = "nixpkgs";
        type = "github";
        ref = "nixos-unstable";
      };
      U.to = {
        owner = "NixOS";
        repo = "nixpkgs";
        type = "github";
      };

      # until https://github.com/NixOS/flake-registry/pull/33
      nixos-generators.to = {
        owner = "nix-community";
        repo = "nixos-generators";
        type = "github";
      };
      nixos-shell.to = {
        owner = "Mic92";
        repo = "nixos-shell";
        type = "github";
      };
      nix-mode.to = {
        owner = "NixOS";
        repo = "nix-mode";
        type = "github";
      };
      microvm.to = {
        owner = "astro";
        repo = "microvm.nix";
        type = "github";
      };
      std.to = {
        owner = "divnix";
        repo = "std";
        type = "github";
      };
      # until https://github.com/NixOS/flake-registry/pull/41 is merged
      haumea.to = {
        owner = "nix-community";
        repo = "haumea";
        type = "github";
      };
    };
  };
}
