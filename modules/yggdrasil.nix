{ config, lib, ... }:

{

  services.yggdrasil = {
    group = "wheel";
    openMulticastPort = true;
    settings = {
      IfName = "ygg0";
      NodeInfo = { };
      NodeInfoPrivacy = true;
    };
  };

  environment.variables.IPFS_GATEWAY = lib.mkIf config.services.yggdrasil.enable "http://ipfs.ygg";

}
