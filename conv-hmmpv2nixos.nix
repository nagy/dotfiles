# A function to convert an evaluated home-manager config of mpv
# to a compatible nixos module
evalhmmodule: hmmodule:

{ pkgs, lib, config, ... }:
let hmconfig = evalhmmodule hmmodule pkgs;
in with lib; {
  # write the config files from  ~/.config/mpv into /etc
  environment = mkIf config.services.xserver.enable {
    etc."mpv/input.conf".text =
      mkIf (hmconfig.xdg.configFile ? "mpv/input.conf")
      hmconfig.xdg.configFile."mpv/input.conf".text;
    etc."mpv/mpv.conf".text = mkIf (hmconfig.xdg.configFile ? "mpv/mpv.conf")
      hmconfig.xdg.configFile."mpv/mpv.conf".text;
    systemPackages = [ hmconfig.programs.mpv.package ];
    variables.MPV_HOME = "/etc/mpv";
  };
}
