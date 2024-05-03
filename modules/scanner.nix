{ pkgs, ... }:

{
  hardware.sane.enable = true;
  users.users.user.extraGroups = [ "scanner" ];

  hardware.sane.backends-package = pkgs.sane-backends.overrideAttrs (
    {
      configureFlags ? [ ],
      ...
    }:
    {
      configureFlags = configureFlags ++ [ "--disable-locking" ];
    }
  );
}
