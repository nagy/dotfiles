{
  haumea ? import <haumea> { },
}:

{
  modules = haumea.load {
    src = ./modules;
    loader = haumea.loaders.verbatim;
  };

  live = import ./nixos-tmpfs-root.nix { };
  live-graphical = import ./nixos-tmpfs-root.nix { moreImports = [ ./modules/desktop.nix ]; };
}
