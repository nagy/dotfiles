{ ... }:

{
  users.users.test = {
    isNormalUser = true;
    uid = 1001;
    extraGroups = [ "video" "render" "scanner" "lp" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZNW8uX6gKASOT+0XXKF2QmeXqMZfoEMIYFogbUF4jo"
    ];
  };
}
