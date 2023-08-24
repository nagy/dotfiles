# A function to convert an evaluated home-manager config of readline to a
# compatible nixos module
evalhmmodule: hmmodule:
{ lib, ... }:

let hmconfig = evalhmmodule hmmodule;
in assert (hmconfig.programs.readline.includeSystemConfig == false); {
  # write the config file from ~/.inputrc into /etc
  environment.etc."inputrc".text = lib.mkIf (hmconfig.home.file ? ".inputrc")
    hmconfig.home.file.".inputrc".text;
}
