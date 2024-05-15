{
  haumea ? import <haumea> { },
}:

{
  modules = haumea.load {
    src = ./modules;
    loader = haumea.loaders.verbatim;
  };
}
