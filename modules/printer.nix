{
  config,
  pkgs,
  lib,
  ...
}:

{

  services.printing = {
    enable = true;
    drivers = [ pkgs.samsung-unified-linux-driver ];
  };
  users.users.user.extraGroups = [ "lp" ];

  # hardware.printers = lib.mkIf config.services.printing.enable {
  #   ensureDefaultPrinter = "SamsungPrinter";
  #   ensurePrinters = [
  #     {
  #       name = "SamsungPrinter";
  #       model = "samsung/CLP-360.ppd";
  #       ppdOptions = {
  #         PageSize = "A4";
  #         ColorModel = "Gray";
  #       };
  #       deviceUri = "usb://Samsung/CLP-360%20Series?serial=XXXXXXXXXXXXXXX";
  #     }
  #   ];
  # };

  # nixpkgs.config.allowUnfree = true; # for samsung driver
  nixpkgs.config.allowUnfreePredicate =
    pkg: lib.elem (lib.getName pkg) [ "samsung-UnifiedLinuxDriver" ];
}
