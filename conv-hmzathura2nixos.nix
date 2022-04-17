# A function to convert an evaluated home-manager config of zathura
# to a compatible nixos module
pkgs: hmconfig: {
  # write the config file from  ~/.config/zathura into /etc
  environment.etc."zathurarc".text =
    hmconfig.xdg.configFile."zathura/zathurarc".text;
  environment.systemPackages = [ hmconfig.programs.zathura.package ];
}
