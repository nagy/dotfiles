{
  config,
  lib,
  nur,
  ...
}:

{
  environment.systemPackages = lib.mkIf config.services.xserver.enable [
    # Combined package
    (config.services.emacs.package.pkgs.withPackages (
      epkgs:
      lib.attrValues (
        nur.repos.nagy.lib.emacsMakeDirectoryScope {
          path = ../emacs;
          inherit epkgs;
        }
      )
    ))
  ];
}
