{ pkgs, ... }:

{
  users.users.user.extraGroups = [ "scanner" ];

  hardware.sane = {
    enable = true;
    # https://github.com/NixOS/nixpkgs/issues/273280
  };
}
