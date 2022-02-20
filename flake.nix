{
  description = "my dotfiles";

  outputs = { self }: {

    lib.module-common = import ./module-common.nix;
    lib.hmmodule-mpv = import ./hmmodule-mpv.nix;
    lib.module-x86_64-linux = import ./module-x86_64-linux.nix;

  };
}
