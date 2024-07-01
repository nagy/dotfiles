{
  haumea ? import <haumea> { },
}:

{
  modules = haumea.load {
    src = ./modules;
    loader = haumea.loaders.verbatim;
  };

  live = import ./nixos-tmpfs-root.nix { extraModules = [ ]; };
  live-graphical = import ./nixos-tmpfs-root.nix { extraModules = [ ./modules/desktop.nix ]; };
}
