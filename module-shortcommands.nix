{ lib, pkgs, ... }:

with import ./lib.nix { inherit pkgs; }; {

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
    (mkShortCommand "CP" [ "nix" "copy" ])
    (mkShortCommand "Sj" [ "nix" "search" "--json" ])
    (mkShortCommand "Fm" [ "nix" "flake" "metadata" ])
    (mkShortCommand "Fn" [ "nix" "flake" "new" ])
    (mkShortCommand "Fu" [ "nix" "flake" "update" ])
    (mkShortCommand "Fl" [ "nix" "flake" "lock" ])
    (mkShortCommand "Fs" [ "nix" "flake" "show" ])
    (mkShortCommand "Fc" [ "nix" "flake" "clone" ])
    (mkShortCommand "Fa" [ "nix" "flake" "archive" ])
    (mkShortCommand "Fsj" [ "nix" "flake" "show" "--json" ])
    (mkShortCommand "Fmj" [ "nix" "flake" "metadata" "--json" ])

    # git
    (mkShortCommand "G" [ "git" ])

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
    (mkShortCommand "dOi" [ "docker" "image" ])
    (mkShortCommand "dOv" [ "docker" "volume" ])
    (mkShortCommand "dOn" [ "docker" "network" ])

    # kubernetes
    (mkShortCommand "K" [ "kubectl" ])
    (mkShortCommand "Kg" [ "kubectl" "get" ])
    (mkShortCommand "Kgp" [ "kubectl" "get" "pod" ])
    (mkShortCommand "Kgd" [ "kubectl" "get" "deployment" ])
    (mkShortCommand "Kgn" [ "kubectl" "get" "node" ])
    (mkShortCommand "Kgpw" [ "kubectl" "get" "pod" "--watch"])
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

    # misc
    (mkShortCommand "jqM" [ "jq" "--monochrome-output" ])
    (mkShortCommand "yqP" [ "yq" "--prettyPrint" ])

    # systemctl
    (mkShortCommand "scs" [ "systemctl" "status" ])
    (mkShortCommand "sclt" [ "systemctl" "list-timers" ])
    (mkShortCommand "scls" [ "systemctl" "list-sockets" ])
    (mkShortCommand "scjlt" [ "systemctl" "--output=json" "list-timers" ])
    (mkShortCommand "scjls" [ "systemctl" "--output=json" "list-sockets" ])
    (mkShortCommand "jc" [ "journalctl" ])
    (mkShortCommand "jcf" [ "journalctl" "-f" ])
    (mkShortCommand "bcj" [ "busctl" "--json" ])
    (mkShortCommand "bcuj" [ "busctl" "--user" "--json" ])

  ];
}