{ pkgs, ... }:

{
  homeconfig.programs.zathura = {
    enable = true;
    package = (pkgs.zathuraPkgs.override { useMupdf = false; }).zathuraWrapper;
    options = {
      render-loading = false;
      dbus-raise-window = false;
      database = "null";
      first-page-column = "1";
      # recolor-reverse-video = true; # this prevents image pdfs from being recolored
      # sandbox = "strict"; # this is broken. with this zathura does not show a window
    };
    # for extraconfig:
    #    this is so you can open links in your browser. otherwise seccomp is active
    #    set sandbox none
    extraConfig = ''
      map S feedkeys ":set first-page-column 1"<Return>
      map D feedkeys ":set first-page-column 2"<Return>
      map j scroll full-down
      map k scroll full-up
      map f toggle_page_mode
      map m toggle_statusbar
      map d scroll half-down
      map u scroll half-up
      map <C-j> scroll down
      map <C-j> scroll up
      map , scroll down
      map . scroll up
      map t recolor
      map Q quit
    '';
  };

}
