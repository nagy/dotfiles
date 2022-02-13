{
  description = "my dotfiles";

  outputs = { self }: {

    lib.module-common = import ./module-common.nix;
    lib.module-x86_64-linux = import ./module-x86_64-linux.nix;

  };
}
