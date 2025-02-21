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
    # boot.kernel.sysctl."net.ipv6.conf.br0.addr_gen_mode" = 1;
    # TODO make this dependent on `networking.bridges.br0.interfaces`.
    boot.kernel.sysctl."net.ipv6.conf.eth0.addr_gen_mode" = 1;
    # boot.kernel.sysctl."net.ipv6.conf.wlp4s0.addr_gen_mode" = 1;

    # networking.dhcpcd.extraConfig = ''
    #   nohook resolv.conf # dont change DNS
    #   # https://wiki.archlinux.org/title/Dhcpcd
    #   noarp
    # ''; # noipv4

    networking.useDHCP = false;
    networking.defaultGateway = "192.168.0.1";
    # networking.bridges.br0.interfaces = [ "eth0" ];
    networking.interfaces.eth0 = {
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
