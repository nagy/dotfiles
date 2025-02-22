{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.etc."nixos-options.json" = lib.mkIf config.documentation.nixos.enable {
    source = "${config.system.build.manual.optionsJSON}/share/doc/nixos/options.json";
  };

  environment.etc."nix-search.json" = lib.mkIf config.documentation.nixos.enable {
    source =
      pkgs.runCommandLocal "nix-search.json"
        {
          nativeBuildInputs = [
            # config.nix.package
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
    nixPath = [
      "nixpkgs=${lib.cleanSource pkgs.path}"
      # "dot=${lib.cleanSource ../.}"
      "jsonrpcrun=${lib.cleanSource <jsonrpcrun>}"
      "nur=${lib.cleanSource <nur>}"
    ];

    registry = {
      nixpkgs.flake = lib.cleanSource pkgs.path;
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
