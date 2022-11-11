{ ... }:

{
  hardware.sane.enable = true;
  users.users.user.extraGroups = [ "scanner" ];
}
