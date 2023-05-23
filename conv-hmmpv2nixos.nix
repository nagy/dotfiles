# A function to convert an evaluated home-manager config of mpv
# to a compatible nixos module
evalhmmodule: hmmodule:

{ lib, config, ... }:
let hmconfig = evalhmmodule hmmodule;
in {
  # write the config files from  ~/.config/mpv into /etc
  environment = lib.mkIf config.services.xserver.enable {
    etc."mpv/input.conf".text =
      lib.mkIf (hmconfig.xdg.configFile ? "mpv/input.conf")
      hmconfig.xdg.configFile."mpv/input.conf".text;
    etc."mpv/mpv.conf".text =
      lib.mkIf (hmconfig.xdg.configFile ? "mpv/mpv.conf")
      hmconfig.xdg.configFile."mpv/mpv.conf".text;
    systemPackages = [ hmconfig.programs.mpv.package ];
    variables.MPV_HOME = "/etc/mpv";
  };
}
