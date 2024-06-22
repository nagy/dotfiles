{ config, ... }:

{
  users.users.test = {
    isNormalUser = true;
    uid = 1001;
    openssh.authorizedKeys.keys = config.users.users.user.openssh.authorizedKeys.keys;
  };
}
