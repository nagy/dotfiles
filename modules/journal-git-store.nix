{ pkgs, ... }:

{
  systemd.services.journal-logger = {
    description = "Logs the journal";
    path = [
      pkgs.jq
      pkgs.inetutils
    ];
    # after = [ "systemd-journald.socket" ];
    # FIXME still one final store before poweroff/reboot is needed
    # this does not work
    # before = [ "poweroff.target" "reboot.target" "halt.target" ];
    # wantedBy = [ "reboot.target" "halt.target" "poweroff.target" ];

    serviceConfig = {
      ExecStart = pkgs.writeShellScript "journal-file-store" ''
        set -euo pipefail
        BOOTID="$(tr -d - < /proc/sys/kernel/random/boot_id)"
        HOSTNAME="$(hostname --short)"
        journalctl --boot="$BOOTID" --output=json \
          | jq --compact-output --sort-keys > ${"$"}{HOSTNAME}_${"$"}{BOOTID}.journal.json
      '';
      WorkingDirectory = "/var/lib/journalgit/";
      BindReadOnlyPaths = [
        "/dev/log"
        "/run/log/journal/" # non-persistent
        "/var/log/journal/" # persistent
      ];
      Type = "oneshot";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateNetwork = true;
      # DynamicUser = true;
      # Group = "systemd-journal"; # to allow read-access
    };
  };
  systemd.timers.journal-logger = {
    wantedBy = [ "timers.target" ];
    # execute every hour
    timerConfig.OnCalendar = "*:00";
    # and once after the unit starts
    timerConfig.OnActiveSec = 0;
  };
}
