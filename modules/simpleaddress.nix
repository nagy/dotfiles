# choose a simple address based on the hostname

{ config, lib, ... }:

let
  numToStringAttr = lib.pipe (lib.splitString "" "abcdefghijklmnopqrstuvwxyz") [
    (lib.imap0 (
      index: value: {
        name = value;
        value = toString index;
      }
    ))
    lib.listToAttrs
  ];
in
{

  config = lib.mkIf (1 == (lib.stringLength config.networking.hostName)) {
    networking.bridges.br0.interfaces = [ "eth0" ];
    networking.interfaces.br0 = {
      ipv4.addresses = [
        {
          address = "192.168.0.${numToStringAttr.${config.networking.hostName}}";
          prefixLength = 24;
        }
      ];
    };
    networking.hosts = lib.pipe 26 [
      (lib.genList (x: x + 1))
      (map (it: {
        int = it;
        str = (lib.elemAt (lib.splitString "" "abcdefghijklmnopqrstuvwxyz") it);
      }))
      (map (x: {
        "192.168.0.${toString x.int}" = [ x.str ];
      }))
      (lib.foldl lib.mergeAttrs { })
    ];
  };

}
