{ pkgs, ... }:

{

  environment.systemPackages = [ pkgs.ncdu ];

  # last line could also be achieved with env NO_COLOR=1
  environment.etc."ncdu.conf".text = ''
    --disable-delete
    --disable-shell
    --shared-column off
    --show-itemcount
    --apparent-size
    --one-file-system
    --exclude-kernfs
    --color off
  '';
}
