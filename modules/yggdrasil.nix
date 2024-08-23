{
  config,
  lib,
  pkgs,
  ...
}:

let
  readYggdrasilOutput =
    name:
    builtins.readFile (
      pkgs.runCommandLocal "yggdrasil-output-${name}.txt" {
        nativeBuildInputs = [ pkgs.yggdrasil ];
        configfile = builtins.toJSON config.services.yggdrasil.settings;
        passAsFile = [ "configfile" ];
      } ''yggdrasil -useconffile "$configfilePath" -${name}|tr -d $'\n'> $out''
    );
in
{

  options = {
    nagy.yggdrasil.addressOutput = lib.mkOption {
      type = lib.types.str;
      default = readYggdrasilOutput "address";
      readOnly = true;
    };
    nagy.yggdrasil.subnetOutput = lib.mkOption {
      type = lib.types.str;
      default = readYggdrasilOutput "subnet";
      readOnly = true;
    };
    nagy.yggdrasil.publickeyOutput = lib.mkOption {
      type = lib.types.str;
      default = readYggdrasilOutput "publickey";
      readOnly = true;
    };
  };

  config = lib.mkIf config.services.yggdrasil.enable {
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
  };
}
