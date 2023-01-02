{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.nagy.shortcommands;
  defaultShortcommands = {
    # nix
    R = [ "nix" "run" ];
    D = [ "nix" "develop" ];
    L = [ "nix" "log" ];
    S = [ "nix" "search" ];
    B = [ "nix" "build" ];
    E = [ "nix" "eval" ];
    F = [ "nix" "flake" ];
    P = [ "nix" "profile" ];
    SH = [ "nix" "shell" ];
    H = [ "nix" "hash" ];
    run = [ "nix" "run" ];
    flake = [ "nix" "flake" ];
    BL = [ "nix" "build" "--print-build-logs" "-j" "1" ];
    RL = [ "nix" "run" "--print-build-logs" "-j" "1" ];
    DL = [ "nix" "develop" "--print-build-logs" "-j" "1" ];
    Ej = [ "nix" "eval" "--json" ];
    Er = [ "nix" "eval" "--raw" ];
    Bj = [ "nix" "build" "--json" "--no-link" ];
    Du = [ "nix" "develop" "--unpack" ];
    Dc = [ "nix" "develop" "--configure" ];
    Db = [ "nix" "develop" "--build" ];
    Di = [ "nix" "develop" "--install" ];
    DC = [ "nix" "develop" "--command" ];
    Pl = [ "nix" "profile" "list" ];
    Pi = [ "nix" "profile" "install" ];
    Pu = [ "nix" "profile" "upgrade" ];
    NA = [ "nix" "nar" ];
    NAl = [ "nix" "nar" "ls" ];
    NAd = [ "nix" "nar" "dump-path" ];
    NAc = [ "nix" "nar" "cat" ];
    CP = [ "nix" "copy" ];
    I = [ "nix" "path-info" ];
    Is = [ "nix" "path-info" "--closure-size" "--human-readable" ];
    Ij = [ "nix" "path-info" "--closure-size" "--json" ];
    Sj = [ "nix" "search" "--json" ];
    Fm = [ "nix" "flake" "metadata" ];
    Fn = [ "nix" "flake" "new" ];
    Fu = [ "nix" "flake" "update" ];
    Fl = [ "nix" "flake" "lock" ];
    Fs = [ "nix" "flake" "show" ];
    Fcl = [ "nix" "flake" "clone" ];
    Fc = [ "nix" "flake" "check" ];
    Fa = [ "nix" "flake" "archive" ];
    Faj = [ "nix" "flake" "archive" "--json" ];
    Fp = [ "nix" "flake" "prefetch" ];
    Fpj = [ "nix" "flake" "prefetch" "--json" ];
    Fsj = [ "nix" "flake" "show" "--json" ];
    Fmj = [ "nix" "flake" "metadata" "--json" ];
    Floin = [
      "nix"
      "flake"
      "lock"
      "--override-input"
      "nixpkgs"
      "nixpkgs/nixos-unstable"
    ];
    Floinuin = [
      "nix"
      "flake"
      "lock"
      "--override-input"
      "nixpkgs"
      "nixpkgs/nixos-unstable"
      "--update-input"
      "nixpkgs"
    ];
    Fuoin = [
      "nix"
      "flake"
      "update"
      "--override-input"
      "nixpkgs"
      "nixpkgs/nixos-unstable"
    ];

    "Rø" = [ "nix" "run" "--override-input" "nixpkgs" "nixpkgs" ];
    "Bø" = [ "nix" "build" "--override-input" "nixpkgs" "nixpkgs" ];
    "Dø" = [ "nix" "develop" "--override-input" "nixpkgs" "nixpkgs" ];
    "Fsø" = [ "nix" "flake" "show" "--override-input" "nixpkgs" "nixpkgs" ];
    "Fmø" = [ "nix" "flake" "metadata" "--override-input" "nixpkgs" "nixpkgs" ];
    "Fcø" = [ "nix" "flake" "check" "--override-input" "nixpkgs" "nixpkgs" ];
    "SHø" = [ "nix" "shell" "--override-input" "nixpkgs" "nixpkgs" ];
    "Pø" = [ "nix" "profile" "--override-input" "nixpkgs" "nixpkgs" ];

    "B." = [ "nix" "build" "--file" "." ];
    "B.j" = [ "nix" "build" "--file" "." "--json" "--no-link" ];
    "R." = [ "nix" "run" "--file" "." ];
    "SH." = [ "nix" "shell" "--file" "." ];
    "E." = [ "nix" "eval" "--file" "." ];
    "E.j" = [ "nix" "eval" "--file" "." "--json" ];
    "S." = [ "nix" "search" "--file" "." ];

    # git
    G = [ "git" ];
    Gcl = [ "git" "clone" ];
    Gcl1 = [ "git" "clone" "--depth=1" ];
    Gf = [ "git" "fetch" ];
    Gfp = [ "git" "fetch" "--prune" ];
    Gt = [ "git" "tag" ];
    Gtl = [ "git" "tag" "--list" ];
    Gts = [ "git" "tags" ];
    Gp = [ "git" "push" ];
    Gpf = [ "git" "push" "--force" ];
    Gpl = [ "git" "pull" ];
    # docker
    # (mkShortCommand "dO" [ "docker" ])
    # (mkShortCommand "dOe" [ "docker" "exec" ])
    # (mkShortCommand "dOeit" [ "docker" "exec" "-it" ])
    # (mkShortCommand "dOr" [ "docker" "run" ])
    # (mkShortCommand "dOc" [ "docker" "container" ])
    # (mkShortCommand "dOcl" [ "docker" "container" "ls" ])
    # (mkShortCommand "dOi" [ "docker" "image" ])
    # (mkShortCommand "dOil" [ "docker" "image" "ls" ])
    # (mkShortCommand "dOv" [ "docker" "volume" ])
    # (mkShortCommand "dOvl" [ "docker" "volume" "ls" ])
    # (mkShortCommand "dOn" [ "docker" "network" ])
    # (mkShortCommand "dOnl" [ "docker" "network" "ls" ])

    # kubernetes
    # (mkShortCommand "K" [ "kubectl" ])
    # (mkShortCommand "Kg" [ "kubectl" "get" ])
    # (mkShortCommand "Kgp" [ "kubectl" "get" "pod" ])
    # (mkShortCommand "Kgd" [ "kubectl" "get" "deployment" ])
    # (mkShortCommand "Kgn" [ "kubectl" "get" "node" ])
    # (mkShortCommand "Kgpw" [ "kubectl" "get" "pod" "--watch" ])
    # (mkShortCommand "Kgdw" [ "kubectl" "get" "deployment" "--watch" ])
    # (mkShortCommand "Kgnw" [ "kubectl" "get" "node" "--watch" ])
    # (mkShortCommand "Kd" [ "kubectl" "describe" ])
    # (mkShortCommand "Kdp" [ "kubectl" "describe" "pod" ])
    # (mkShortCommand "Kdd" [ "kubectl" "describe" "deployment" ])
    # (mkShortCommand "Kdn" [ "kubectl" "describe" "node" ])
    # (mkShortCommand "Kc" [ "kubectl" "create" ])
    # (mkShortCommand "Kcp" [ "kubectl" "create" "pod" ])
    # (mkShortCommand "Kcd" [ "kubectl" "create" "deployment" ])
    # (mkShortCommand "Kcj" [ "kubectl" "create" "job" ])
    # (mkShortCommand "Ke" [ "kubectl" "exec" ])
    # (mkShortCommand "Keit" [ "kubectl" "exec" "-it" ])
    # (mkShortCommand "Kl" [ "kubectl" "label" ])

    # npm
    nPb = [ "npm" "run" "build" ];
    nPt = [ "npm" "run" "test" ];
    nPs = [ "npm" "run" "start" ];
    nPi = [ "npm" "install" ];
    nPci = [ "npm" "ci" ];
    nPu = [ "npm" "update" ];

    # systemctl
    scs = [ "systemctl" "status" ];
    sclt = [ "systemctl" "list-timers" ];
    scls = [ "systemctl" "list-sockets" ];
    scjlt = [ "systemctl" "--output=json" "list-timers" ];
    scjls = [ "systemctl" "--output=json" "list-sockets" ];
    jF = [ "journalctl" "-f" ];
    JJf = [ "journalctl" "-f" ];
    bcj = [ "busctl" "--json" ];
    bcc = [ "busctl" "call" ];
    bci = [ "busctl" "introspect" ];
    bcu = [ "busctl" "--user" ];
    bcuj = [ "busctl" "--user" "--json" ];
    bcuc = [ "busctl" "--user" "call" ];
    bcui = [ "busctl" "--user" "introspect" ];

    # misc
    J = [ "jq" "--monochrome-output" ];
    jqM = [ "jq" "--monochrome-output" ];
    Jr = [ "jq" "--monochrome-output" "--raw-output" ];
    Y = [ "yq" "--prettyPrint" "--no-colors" ];
    yqP = [ "yq" "--prettyPrint" ];
    yqPM = [ "yq" "--prettyPrint" "--no-colors" ];
    yqMP = [ "yq" "--no-colors" "--prettyPrint" ];
    yqM = [ "yq" "--no-colors" ];
    cpa = [ "cp" "--archive" ];
    rmf = [ "rm" "--force" ];
    i4 = [ "ip" "-4" ];
    i6 = [ "ip" "-6" ];
    ij = [ "ip" "--json" ];
    i4j = [ "ip" "-4" "--json" ];
    i6j = [ "ip" "-6" "--json" ];
    "⬡" = [ "hexdump" "--no-squeezing" "--canonical" ];
    "⬡n" = [ "hexdump" "--no-squeezing" "--canonical" "--length" ];
  };
in with import ../lib pkgs; {

  options = {
    nagy.shortcommands = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = { };
      description = "shortcommands";
    };
  };

  config = {

    environment.systemPackages =
      lib.mapAttrsToList mkShortCommand (defaultShortcommands // cfg);

  };

}
