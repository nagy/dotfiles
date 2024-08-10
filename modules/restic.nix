{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption;
  inherit (lib.types)
    str
    attrsOf
    listOf
    attrs
    package
    ;
  cfg = config.nagy.restic;

  mkResticWrapper =
    name:
    {
      repo,
      shortcut ? null,
      runtimeEnv ? { },
      from ? null,
    }:
    pkgs.writeShellApplication {
      name = "restic-${name}";
      runtimeInputs = [ pkgs.restic ];
      runtimeEnv =
        {
          RESTIC_READ_CONCURRENCY = 1;
          RESTIC_REPOSITORY = repo;
          RESTIC_CACHE_DIR = "/tmp/restic-cache-${name}";
        }
        // cfg.defaultRuntimeEnv
        // (lib.optionalAttrs (from != null) { RESTIC_FROM_REPOSITORY = from.repo; })
        // (if (from != null) then from.runtimeEnv else { })
        // runtimeEnv;
      derivationArgs.passthru = {
        inherit repo from runtimeEnv;
        shortcommands = lib.optionalAttrs (shortcut != null) {
          "📀${shortcut}" = [ "restic-${name}" ];
          "📀${shortcut}b" = [
            "restic-${name}"
            "backup"
          ];
          "📀${shortcut}s" = [
            "restic-${name}"
            "snapshots"
            "--no-lock"
          ];
          "📀${shortcut}sj" = [
            "restic-${name}"
            "snapshots"
            "--json"
            "--no-lock"
          ];
        };
      };
      text = ''
        exec restic "$@"
      '';
    };
in
{
  imports = [ ./shortcommands.nix ];

  options.nagy.restic = {
    attrs = mkOption {
      type = attrsOf attrs;
      default = { };
      description = "restic attrs";
    };
    packages = mkOption {
      type = listOf package;
      default = lib.mapAttrsToList mkResticWrapper cfg.attrs;
      description = "restic package attributes";
      readOnly = true;
    };
    packagesAttr = mkOption {
      type = attrsOf package;
      default = lib.mapAttrs mkResticWrapper cfg.attrs;
      description = "restic packages";
      readOnly = true;
    };
    defaultRuntimeEnv = mkOption {
      type = attrsOf str;
      default = { };
      example = {
        RESTIC_PASSWORD_COMMAND = "pass show restic";
      };
      description = "environment variables added to every package";
    };
  };

  config = {
    environment.systemPackages = cfg.packages;
    nagy.shortcommands = lib.mergeAttrsList (map (x: x.shortcommands) cfg.packages);
  };
}
