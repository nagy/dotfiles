# A function to convert an evaluated home-manager config of zathura
# to a compatible nixos module
pkgs: hmconfig: {
  # write the config file from  ~/.config/zathura into /etc
  environment.etc."zathurarc".text =
    hmconfig.xdg.configFile."zathura/zathurarc".text;
  # And add only evaluated packages with prefix zathura
  environment.systemPackages =
    pkgs.lib.filter (x: pkgs.lib.hasPrefix "zathura" x.name) hmconfig.home.packages;
}
