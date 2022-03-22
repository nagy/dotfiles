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

    # kubernetes
    (mkShortCommand "K" [ "kubectl" ])
    (mkShortCommand "Kg" [ "kubectl" "get" ])
    (mkShortCommand "Kgp" [ "kubectl" "get" "pod" ])
    (mkShortCommand "Kgd" [ "kubectl" "get" "deployment" ])
    (mkShortCommand "Kgn" [ "kubectl" "get" "node" ])
    (mkShortCommand "Kd" [ "kubectl" "describe" ])
    (mkShortCommand "Kdp" [ "kubectl" "describe" "pod" ])
    (mkShortCommand "Kdd" [ "kubectl" "describe" "deployment" ])
    (mkShortCommand "Kdn" [ "kubectl" "describe" "node" ])
    (mkShortCommand "Kl" [ "kubectl" "label" ])

    # misc
    (mkShortCommand "jqM" [ "jq" "--monochrome-output" ])
    (mkShortCommand "yqP" [ "yq" "--prettyPrint" ])

    # systemctl
    (mkShortCommand "scs" [ "systemctl" "status" ])
    (mkShortCommand "sclt" [ "systemctl" "list-timers" ])
    (mkShortCommand "scls" [ "systemctl" "list-sockets" ])
    (mkShortCommand "scjlt" [ "systemctl" "--output=json" "list-timers" ])
    (mkShortCommand "scjls" [ "systemctl" "--output=json" "list-sockets" ])
    (mkShortCommand "bcj" [ "busctl" "--json" ])
    (mkShortCommand "bcuj" [ "busctl" "--user" "--json" ])

  ];
}
