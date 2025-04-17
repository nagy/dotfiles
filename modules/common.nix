{ lib, ... }:

{
  users.users.user = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [
      "wheel"
      "dialout"
    ];
  };
  # users.mutableUsers = false; # this can break the manually set password !!!!

  # services.getty.autologinUser = "user";
  # use the force variant until the netboot has been cleaned up
  services.getty.autologinUser = lib.mkForce "user";

  services.openssh = {
    enable = true;
  };

  # https://askubuntu.com/questions/493002/global-sudo-session-in-ubuntu
  security.sudo.extraConfig = ''
    Defaults:user !tty_tickets, timestamp_timeout=60
  '';

  security.sudo = {
    execWheelOnly = true;
  };
}
