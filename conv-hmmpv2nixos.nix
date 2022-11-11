# A function to convert an evaluated home-manager config of mpv
# to a compatible nixos module
{ pkgs, evalhmmodule }:

{ hmmodule,  hmconfig ? evalhmmodule hmmodule }: {
  # write the config files from  ~/.config/mpv into /etc
  environment.etc."mpv/input.conf".text =
    pkgs.lib.mkIf (hmconfig.xdg.configFile ? "mpv/input.conf")
    hmconfig.xdg.configFile."mpv/input.conf".text;
  environment.etc."mpv/mpv.conf".text =
    pkgs.lib.mkIf (hmconfig.xdg.configFile ? "mpv/mpv.conf")
    hmconfig.xdg.configFile."mpv/mpv.conf".text;
  environment.systemPackages = [ hmconfig.programs.mpv.package ];
  environment.variables.MPV_HOME = "/etc/mpv";
}
