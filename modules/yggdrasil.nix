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

  networking.hosts = lib.mkIf config.services.yggdrasil.enable {
    "222:3bd:cc26:9545:caaa:9fd6:ec56:cc1" = [ "y.www.nncpgo.org" ];
  };

}
