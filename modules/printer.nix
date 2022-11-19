{ pkgs, lib, ... }:

{

  services.printing = {
    enable = true;
    drivers = with pkgs; [ samsung-unified-linux-driver ];
  };
  users.users.user.extraGroups = [ "lp" ];
  hardware.printers.ensureDefaultPrinter = "SamsungPrinter";
  # hardware.printers.ensurePrinters = [{
  #   name = "SamsungPrinter";
  #   model = "samsung/CLP-360.ppd";
  #   ppdOptions = {
  #     PageSize = "A4";
  #     ColorModel = "Gray";
  #   };
  #   deviceUri = "usb://Samsung/CLP-360%20Series?serial=XXXXXXXXXXXXXXX";
  # }];

  # nixpkgs.config.allowUnfree = true; # for samsung driver
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "samsung-UnifiedLinuxDriver" ];
}
