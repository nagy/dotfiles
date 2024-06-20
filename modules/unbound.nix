{
  networking.nameservers = [ "127.0.0.1" ];
  # Upstream DoH is not supported yet
  # https://github.com/NLnetLabs/unbound/issues/308
  services.unbound = {
    enable = true;
    resolveLocalQueries = false; # because we dont use 127.0.0.1
    # man:unbound.conf
    settings = {
      # localControlSocketPath = "/run/unbound/unbound.ctl";
      forward-zone = [
        {
          name = ".";
          forward-tls-upstream = true;
          forward-addr = [
            "1.1.1.1@853#cloudflare-dns.com"
            "1.0.0.1@853#cloudflare-dns.com"
          ];
        }
      ];
      server = {
        log-queries = true;
        log-replies = true;
        hide-identity = true;
        hide-version = true;
        deny-any = true;
      };
    };
  };
}
