{ nur, ... }:

{
  imports = [
    nur.repos.nagy.modules.ssh

    ./common.nix
    ./converter.nix
    # ./desktop.nix
    ./display.nix
    ./emacs.nix
    ./firefox.nix
    ./fonts.nix
    ./fzf.nix
    ./git.nix
    ./gtk.nix
    ./htop.nix
    # ./journal-git-store.nix
    ./ncdu.nix
    ./nix.nix
    # ./printer.nix
    # ./rust.nix
    # ./scanner.nix
    ./simpleaddress.nix
    ./shortcommands.nix
    ./starship.nix
    ./sudo.nix
    ./yggdrasil.nix
    # ./systemd-resolved.nix
    # ./test-user.nix
    ./xcompose.nix
    ./simpleaddress.nix
    ./espressif.nix
    ./restic.nix
    # ./automount-ext4.nix
    # ./automount-btrfs.nix

    ./hmmodule-mpv.nix
    ./hmmodule-readline.nix
    ./hmmodule-zathura.nix
  ];
}
