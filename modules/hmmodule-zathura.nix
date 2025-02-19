{
  config,
  pkgs,
  nur,
  ...
}:

{
  # imports = [ nur.repos.nagy.modules.hmconvert ];

  homeconfig.programs.zathura = {
    enable = config.services.xserver.enable;
    # package = (pkgs.zathuraPkgs.override { useMupdf = false; }).zathuraWrapper;
    options = {
      render-loading = false;
      dbus-raise-window = false;
      database = "null";
      first-page-column = "1";
      selection-clipboard = "clipboard";
      pages-per-row = 2;
      # recolor-reverse-video = true; # this prevents image pdfs from being recolored
      # sandbox = "strict";
    };
    # for extraconfig:
    #    this is so you can open links in your browser. otherwise seccomp is active
    #    set sandbox none
    extraConfig = ''
      map S feedkeys ":set first-page-column 1"<Return>
      map D feedkeys ":set first-page-column 2"<Return>
      map f toggle_page_mode
      map m toggle_statusbar
      map d scroll half-down
      map u scroll half-up
      map ^j scroll full-down
      map ^k scroll full-up
      map , scroll full-down
      map . scroll full-up
      map t recolor
      map Q quit
    '';
  };
}
