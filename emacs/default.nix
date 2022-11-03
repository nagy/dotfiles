{ lib, trivialBuild, use-package, elpher }:

trivialBuild {
  pname = "nagy";
  version = "unstable";
  src = lib.cleanSource ./.;
  packageRequires = [ use-package elpher ];
}
