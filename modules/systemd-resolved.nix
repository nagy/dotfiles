# https://nixos.wiki/wiki/Systemd-resolved
{

  services.resolved = {
    enable = true;
    # does not work
    # dnssec = "true";
    domains = [ "~." ];
    fallbackDns = [
      "1.1.1.1#one.one.one.one"
      "1.0.0.1#one.one.one.one"
    ];
    dnsovertls = "true";
  };
}
