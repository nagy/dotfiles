# A function to convert an evaluated home-manager config of readline to a
# compatible nixos module
# WARNING:
#   THE MODULE NEEDS TO HAVE `programs.readline.includeSystemConfig` DISABLED
#   BEFORE EVALUATION
evalhmmodule: hmmodule:

{ pkgs, lib, config, ... }:
let hmconfig = evalhmmodule hmmodule pkgs;
in with lib; {
  # write the config file from ~/.inputrc into /etc
  environment.etc."inputrc".text =
    mkIf (hmconfig.home.file ? ".inputrc") hmconfig.home.file.".inputrc".text;
}
