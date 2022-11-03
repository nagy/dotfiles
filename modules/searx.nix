{ pkgs, ... }:

{
  services.searx = {
    enable = true;
    package = pkgs.searxng;
    # runInUwsgi = true;
    settings = {
      server.bind_address = "0.0.0.0";
      server.secret_key = "veryultrasecret";
      server.image_proxy = true;
      search.formats = [ "html" "json" ];
      ui.query_in_title = true;
    };
  };
}
