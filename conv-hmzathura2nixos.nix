# A function to convert an evaluated home-manager config of zathura
# to a compatible nixos module
evalhmmodule: hmmodule:

{ lib, config, ... }:
let hmconfig = evalhmmodule hmmodule;
in {
  # write the config file from  ~/.config/zathura into /etc
  environment = lib.mkIf config.services.xserver.enable {
    etc."zathurarc".text = hmconfig.xdg.configFile."zathura/zathurarc".text;
    systemPackages = [ hmconfig.programs.zathura.package ];
  };
}
