# A function to convert an evaluated home-manager config of readline to a
# compatible nixos module
# WARNING:
#   THE MODULE NEEDS TO HAVE `programs.readline.includeSystemConfig` DISABLED
#   BEFORE EVALUATION
pkgs: hmconfig: {
  # write the config file from ~/.inputrc into /etc
  environment.etc."inputrc".text = hmconfig.home.file.".inputrc".text;
}
