{
  config,
  lib,
  ...
}:

let
  cfg = config.services.yggdrasil;
  port = 9001;
in
{
  services.yggdrasil = {
    group = "wheel";
    openMulticastPort = true;
    settings = {
      IfName = "ygg0";
      # fix port
      MulticastInterfaces = [
        {
          Regex = ".*";
          Beacon = true;
          Listen = true;
          Port = port;
          Priority = 0;
          Password = "";
        }
      ];
    };
  };
  # environment.variables.IPFS_GATEWAY = lib.mkIf cfg.enable "http://ipfs.ygg";

}
