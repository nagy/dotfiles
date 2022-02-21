{ pkgs, font, fontSize, ... }:

{
  programs.zathura = {
    enable = true;
    options = {
      font = "${font} ${toString fontSize}";
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
      map Q quit
    '';
  };

}
