{ pkgs, ... }:

{
  # inspired from https://gist.github.com/juancarlospaco/7f4eab1b6899c55ea90dc0ef5eea965d
  # man:systemd-mount
  # man:udev
  services.udev.extraRules = ''
    ACTION=="add|change" \
    , SUBSYSTEM=="block" \
    , ENV{ID_FS_USAGE}=="filesystem" \
    , ENV{ID_FS_TYPE}=="ext4" \
    , RUN{program}+="${pkgs.systemd}/bin/systemd-mount --no-block --automount=yes --options=noatime,rw --collect $devnode /run/media/system/$env{ID_FS_UUID}"

    ACTION=="remove" \
    , SUBSYSTEM=="block" \
    , ENV{ID_FS_USAGE}=="filesystem" \
    , ENV{ID_FS_TYPE}=="ext4" \
    , RUN{program}+="${pkgs.systemd}/bin/systemd-mount --umount /run/media/system/$env{ID_FS_UUID}"
  '';

}
