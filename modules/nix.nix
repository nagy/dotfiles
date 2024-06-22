{
  config,
  lib,
  pkgs,
  ...
}:

{

  environment.systemPackages = with pkgs; [
    nixfmt-rfc-style
    nil

    nix-prefetch
    nix-prefetch-git
    nix-diff
    nix-init
    nix-update

    nickel
    nls
  ];

  environment.etc."nixos-options.json" = lib.mkIf config.documentation.nixos.enable {
    source = "${config.system.build.manual.optionsJSON}/share/doc/nixos/options.json";
  };

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
      # Build logs are backed up. Backup mechanism itself takes care of the compression already.
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
    };
  };
}
