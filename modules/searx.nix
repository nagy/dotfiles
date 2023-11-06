{ pkgs, ... }:

{
  services.searx = {
    enable = false;
    package = pkgs.searxng;
    # runInUwsgi = true;
    settings = {
      server.bind_address = "127.0.0.1";
      server.secret_key = "veryultrasecret";
      server.image_proxy = true;
      search.formats = [ "html" "json" ];
      ui.query_in_title = true;
    };
  };
}
