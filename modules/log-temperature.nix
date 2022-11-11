{ pkgs, ... }:

{
  systemd.services.temperature-logger =   {
    description = "Logs the temperature";
    # wantedBy = [ "multi-user.target" ];
    unitConfig = {
      ConditionPathExists = "/sys/devices/virtual/thermal/thermal_zone0/hwmon0/device/temp";
    };
    serviceConfig = {
      ExecStart = "${pkgs.coreutils}/bin/cat";
      Type = "oneshot";
      DynamicUser = true;
      StandardInput = "file:/sys/devices/virtual/thermal/thermal_zone0/hwmon0/device/temp";
    };
  };
  systemd.timers.temperature-logger = {
    wantedBy = [ "timers.target" ];
    # execute every 30 seconds
    timerConfig.OnCalendar = "*:*:0/30";
  };
}
