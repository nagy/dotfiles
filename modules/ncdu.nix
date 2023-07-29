{
  # last line could also be achived with env NO_COLOR=1
  environment.etc."ncdu.conf".text = ''
    --disable-delete
    --disable-shell
    --shared-column off
    --show-itemcount
    --apparent-size
    --one-file-system
    --color off
  '';
}
