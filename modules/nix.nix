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
    nvd
    nix-du
    nix-tree
    nix-init
    nix-update
    nix-output-monitor

    nickel
    nls
  ];

  environment.etc."nixos-options.json" = lib.mkIf config.documentation.nixos.enable {
    source = "${config.system.build.manual.optionsJSON}/share/doc/nixos/options.json";
  };

  environment.etc."nix-search.json" = lib.mkIf config.documentation.nixos.enable {
    source =
      pkgs.runCommandLocal "nix-search.json"
        {
          nativeBuildInputs = [
            pkgs.nixVersions.latest
            # optional
            pkgs.jq
          ];
        }
        ''
          echo '{"flakes":[],"version":2}' > empty-registry.json
          nix --offline --store ./. \
            --extra-experimental-features 'nix-command flakes' \
            --option flake-registry $PWD/empty-registry.json \
            search path:${pkgs.path} --json "" | jq --sort-keys > $out
        '';
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

    nixPath = [
      "nixpkgs=${pkgs.path}"
      "dot=${lib.cleanSource ../.}"
      "haumea=${lib.cleanSource <haumea>}"
      "nur=${lib.cleanSource <nur>}"
    ];

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
