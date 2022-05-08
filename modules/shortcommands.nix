{ pkgs, ... }:

with import ../lib.nix pkgs; {

  environment.systemPackages = with pkgs; [
    # nix
    (mkShortCommand "R" [ "nix" "run" ])
    (mkShortCommand "D" [ "nix" "develop" ])
    (mkShortCommand "L" [ "nix" "log" ])
    (mkShortCommand "S" [ "nix" "search" ])
    (mkShortCommand "B" [ "nix" "build" ])
    (mkShortCommand "E" [ "nix" "eval" ])
    (mkShortCommand "F" [ "nix" "flake" ])
    (mkShortCommand "P" [ "nix" "profile" ])
    (mkShortCommand "SH" [ "nix" "shell" ])
    (mkShortCommand "H" [ "nix" "hash" ])
    (mkShortCommand "run" [ "nix" "run" ])
    (mkShortCommand "flake" [ "nix" "flake" ])
    (mkShortCommand "Ej" [ "nix" "eval" "--json" ])
    (mkShortCommand "Er" [ "nix" "eval" "--raw" ])
    (mkShortCommand "Bj" [ "nix" "build" "--json" ])
    (mkShortCommand "Du" [ "nix" "develop" "--unpack" ])
    (mkShortCommand "Dc" [ "nix" "develop" "--configure" ])
    (mkShortCommand "Db" [ "nix" "develop" "--build" ])
    (mkShortCommand "Di" [ "nix" "develop" "--install" ])
    (mkShortCommand "DC" [ "nix" "develop" "--command" ])
    (mkShortCommand "Pl" [ "nix" "profile" "list" ])
    (mkShortCommand "Pi" [ "nix" "profile" "install" ])
    (mkShortCommand "NA" [ "nix" "nar" ])
    (mkShortCommand "NAl" [ "nix" "nar" "ls" ])
    (mkShortCommand "NAd" [ "nix" "nar" "dump-path" ])
    (mkShortCommand "NAc" [ "nix" "nar" "cat" ])
    (mkShortCommand "CP" [ "nix" "copy" ])
    (mkShortCommand "Sj" [ "nix" "search" "--json" ])
    (mkShortCommand "Fm" [ "nix" "flake" "metadata" ])
    (mkShortCommand "Fn" [ "nix" "flake" "new" ])
    (mkShortCommand "Fu" [ "nix" "flake" "update" ])
    (mkShortCommand "Fl" [ "nix" "flake" "lock" ])
    (mkShortCommand "Fs" [ "nix" "flake" "show" ])
    (mkShortCommand "Fc" [ "nix" "flake" "clone" ])
    (mkShortCommand "Fa" [ "nix" "flake" "archive" ])
    (mkShortCommand "Fp" [ "nix" "flake" "prefetch" ])
    (mkShortCommand "Fpj" [ "nix" "flake" "prefetch" "--json" ])
    (mkShortCommand "Fsj" [ "nix" "flake" "show" "--json" ])
    (mkShortCommand "Fmj" [ "nix" "flake" "metadata" "--json" ])
    (mkShortCommand "Floin" [
      "nix"
      "flake"
      "lock"
      "--override-input"
      "nixpkgs"
      "nixpkgs/nixos-unstable"
    ])
    (mkShortCommand "Floinuin" [
      "nix"
      "flake"
      "lock"
      "--override-input"
      "nixpkgs"
      "nixpkgs/nixos-unstable"
      "--update-input"
      "nixpkgs"
    ])
    (mkShortCommand "Fuoin" [
      "nix"
      "flake"
      "update"
      "--override-input"
      "nixpkgs"
      "nixpkgs/nixos-unstable"
    ])

    # git
    (mkShortCommand "G" [ "git" ])
    (mkShortCommand "Gcl" [ "git" "clone" ])
    (mkShortCommand "Gcl1" [ "git" "clone" "--depth=1" ])
    (mkShortCommand "Gf" [ "git" "fetch" ])
    (mkShortCommand "Gfp" [ "git" "fetch" "--prune" ])
    (mkShortCommand "Gt" [ "git" "tag" ])
    (mkShortCommand "Gtl" [ "git" "tag" "--list" ])
    (mkShortCommand "Gts" [ "git" "tags" ])
    (mkShortCommand "Gp" [ "git" "push" ])
    (mkShortCommand "Gpf" [ "git" "push" "--force" ])
    (mkShortCommand "Gpl" [ "git" "pull" ])

    # home-manager
    (mkShortCommand "HM" [ "home-manager" ])
    (mkShortCommand "HMb" [ "home-manager" "build" ])
    (mkShortCommand "HMs" [ "home-manager" "switch" ])
    (mkShortCommand "HMi" [ "home-manager" "instantiate" ])

    # docker
    (mkShortCommand "dO" [ "docker" ])
    (mkShortCommand "dOe" [ "docker" "exec" ])
    (mkShortCommand "dOeit" [ "docker" "exec" "-it" ])
    (mkShortCommand "dOr" [ "docker" "run" ])
    (mkShortCommand "dOc" [ "docker" "container" ])
    (mkShortCommand "dOcl" [ "docker" "container" "ls" ])
    (mkShortCommand "dOi" [ "docker" "image" ])
    (mkShortCommand "dOil" [ "docker" "image" "ls" ])
    (mkShortCommand "dOv" [ "docker" "volume" ])
    (mkShortCommand "dOvl" [ "docker" "volume" "ls" ])
    (mkShortCommand "dOn" [ "docker" "network" ])
    (mkShortCommand "dOnl" [ "docker" "network" "ls" ])

    # kubernetes
    (mkShortCommand "K" [ "kubectl" ])
    (mkShortCommand "Kg" [ "kubectl" "get" ])
    (mkShortCommand "Kgp" [ "kubectl" "get" "pod" ])
    (mkShortCommand "Kgd" [ "kubectl" "get" "deployment" ])
    (mkShortCommand "Kgn" [ "kubectl" "get" "node" ])
    (mkShortCommand "Kgpw" [ "kubectl" "get" "pod" "--watch" ])
    (mkShortCommand "Kgdw" [ "kubectl" "get" "deployment" "--watch" ])
    (mkShortCommand "Kgnw" [ "kubectl" "get" "node" "--watch" ])
    (mkShortCommand "Kd" [ "kubectl" "describe" ])
    (mkShortCommand "Kdp" [ "kubectl" "describe" "pod" ])
    (mkShortCommand "Kdd" [ "kubectl" "describe" "deployment" ])
    (mkShortCommand "Kdn" [ "kubectl" "describe" "node" ])
    (mkShortCommand "Kc" [ "kubectl" "create" ])
    (mkShortCommand "Kcp" [ "kubectl" "create" "pod" ])
    (mkShortCommand "Kcd" [ "kubectl" "create" "deployment" ])
    (mkShortCommand "Kcj" [ "kubectl" "create" "job" ])
    (mkShortCommand "Ke" [ "kubectl" "exec" ])
    (mkShortCommand "Keit" [ "kubectl" "exec" "-it" ])
    (mkShortCommand "Kl" [ "kubectl" "label" ])

    # npm
    (mkShortCommand "nPb" [ "npm" "run" "build" ])
    (mkShortCommand "nPt" [ "npm" "run" "test" ])
    (mkShortCommand "nPi" [ "npm" "install" ])
    (mkShortCommand "nPci" [ "npm" "ci" ])
    (mkShortCommand "nPu" [ "npm" "update" ])

    # misc
    (mkShortCommand "jqM" [ "jq" "--monochrome-output" ])
    (mkShortCommand "yqP" [ "yq" "--prettyPrint" ])
    (mkShortCommand "yqPM" [ "yq" "--prettyPrint" "--no-colors" ])
    (mkShortCommand "yqMP" [ "yq" "--no-colors" "--prettyPrint" ])
    (mkShortCommand "yqM" [ "yq" "--no-colors" ])

    # systemctl
    (mkShortCommand "scs" [ "systemctl" "status" ])
    (mkShortCommand "sclt" [ "systemctl" "list-timers" ])
    (mkShortCommand "scls" [ "systemctl" "list-sockets" ])
    (mkShortCommand "scjlt" [ "systemctl" "--output=json" "list-timers" ])
    (mkShortCommand "scjls" [ "systemctl" "--output=json" "list-sockets" ])
    (mkShortCommand "jc" [ "journalctl" ])
    (mkShortCommand "jcf" [ "journalctl" "-f" ])
    (mkShortCommand "bcj" [ "busctl" "--json" ])
    (mkShortCommand "bcc" [ "busctl" "call" ])
    (mkShortCommand "bci" [ "busctl" "introspect" ])
    (mkShortCommand "bcu" [ "busctl" "--user" ])
    (mkShortCommand "bcuj" [ "busctl" "--user" "--json" ])
    (mkShortCommand "bcuc" [ "busctl" "--user" "call" ])
    (mkShortCommand "bcui" [ "busctl" "--user" "introspect" ])

  ];
}
