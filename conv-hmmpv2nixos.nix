# A function to convert an evaluated home-manager config of mpv
# to a compatible nixos module
pkgs: hmconfig: {
  # write the config files from  ~/.config/mpv into /etc
  # FIXME this may fail if these dont exist.
  environment.etc."mpv/input.conf".text =
    hmconfig.xdg.configFile."mpv/input.conf".text;
  environment.etc."mpv/mpv.conf".text =
    hmconfig.xdg.configFile."mpv/mpv.conf".text;
  # And add only evaluated packages with prefix zathura
  environment.systemPackages =
    pkgs.lib.filter (x: pkgs.lib.hasPrefix "mpv" x.name) hmconfig.home.packages;
  environment.variables.MPV_HOME = "/etc/mpv";
}
