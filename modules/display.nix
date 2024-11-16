{
  config,
  pkgs,
  lib,
  ...
}:

{
  config = # lib.mkIf config.services.xserver.enable
    {
      # https://discourse.nixos.org/t/brightness-control-of-external-monitors-with-ddcci-backlight/8639
      boot.extraModulePackages = [ config.boot.kernelPackages.ddcci-driver ];
      hardware.i2c.enable = true;
      boot.kernelModules = [ "ddcci_backlight" ];
      environment.systemPackages = [ pkgs.brightnessctl ];
    };
}
