{ nur, ... }:

{
  imports = [
    nur.repos.nagy.modules.all

    ./common.nix
    # ./desktop.nix
    ./display.nix
    ./emacs.nix
    ./firefox.nix
    ./fonts.nix
    ./git.nix
    ./hledger.nix
    ./nix.nix
    # ./printer.nix
    ./scanner.nix
    ./shortcommands.nix
    ./starship.nix
    ./sudo.nix
    ./yggdrasil.nix
    ./xcompose.nix
    ./simpleaddress.nix
    ./espressif.nix
    ./powerdns.nix
    ./restic.nix
    # ./automount-ext4.nix
    # ./automount-btrfs.nix

    ./hmmodule-mpv.nix
    ./hmmodule-readline.nix
    ./hmmodule-zathura.nix
  ];
}
