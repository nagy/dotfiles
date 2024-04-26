{ pkgs, lib, config, ... }:

let
  cfg = config.nagy.shortcommands;
  defaultShortcommands = {
    # nix flakes
    n = [ "nix" ];
    b = [ "nix-build" ];
    i = [ "nix-instantiate" ];
    "b," = [ "nix-build" "<nixpkgs>" "--attr" ];
    "i," = [ "nix-instantiate" "<nixpkgs>" "--attr" ];
    R = [ "nix" "run" ];
    SE = [ "nix" "search" ];
    B = [ "nix" "build" ];
    E = [ "nix" "eval" ];
    F = [ "nix" "flake" ];
    P = [ "nix" "profile" ];
    S = [ "nix" "shell" ];
    H = [ "nix" "hash" ];
    BL = [ "nix" "build" "--print-build-logs" ];
    RL = [ "nix" "run" "--print-build-logs" ];
    Ej = [ "nix" "eval" "--json" ];
    Er = [ "nix" "eval" "--raw" ];
    Bj = [ "nix" "build" "--json" "--no-link" ];
    Pl = [ "nix" "profile" "list" ];
    Pi = [ "nix" "profile" "install" ];
    Pu = [ "nix" "profile" "upgrade" ];
    Pr = [ "nix" "profile" "remove" ];
    NA = [ "nix" "nar" ];
    NAl = [ "nix" "nar" "ls" ];
    NAd = [ "nix" "nar" "dump-path" ];
    NAc = [ "nix" "nar" "cat" ];
    CP = [ "nix" "copy" ];
    I = [ "nix" "path-info" ];
    Is = [ "nix" "path-info" "--size" "--human-readable" ];
    IS = [ "nix" "path-info" "--closure-size" "--human-readable" ];
    Ij = [ "nix" "path-info" "--json" ];
    IJ = [ "nix" "path-info" "--closure-size" "--json" ];
    Ia = [ "nix" "path-info" "-rsSh" ];
    SEj = [ "nix" "search" "--json" ];
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

    "Rø" = [ "nix" "run" "--override-input" "nixpkgs" "nixpkgs" ];
    "Bø" = [ "nix" "build" "--override-input" "nixpkgs" "nixpkgs" ];
    "Eø" = [ "nix" "eval" "--override-input" "nixpkgs" "nixpkgs" ];
    "Fsø" = [ "nix" "flake" "show" "--override-input" "nixpkgs" "nixpkgs" ];
    "Fmø" = [ "nix" "flake" "metadata" "--override-input" "nixpkgs" "nixpkgs" ];
    "Fcø" = [ "nix" "flake" "check" "--override-input" "nixpkgs" "nixpkgs" ];
    "Sø" = [ "nix" "shell" "--override-input" "nixpkgs" "nixpkgs" ];

    "RØ" = [
      "nix"
      "run"
      "--override-input"
      "nixpkgs"
      "github:NixOS/nixpkgs/master"
    ];
    "BØ" = [
      "nix"
      "build"
      "--override-input"
      "nixpkgs"
      "github:NixOS/nixpkgs/master"
    ];
    "FsØ" = [
      "nix"
      "flake"
      "show"
      "--override-input"
      "nixpkgs"
      "github:NixOS/nixpkgs/master"
    ];
    "FmØ" = [
      "nix"
      "flake"
      "metadata"
      "--override-input"
      "nixpkgs"
      "github:NixOS/nixpkgs/master"
    ];
    "FcØ" = [
      "nix"
      "flake"
      "check"
      "--override-input"
      "nixpkgs"
      "github:NixOS/nixpkgs/master"
    ];
    "SØ" = [
      "nix"
      "shell"
      "--override-input"
      "nixpkgs"
      "github:NixOS/nixpkgs/master"
    ];
    "PØ" = [
      "nix"
      "profile"
      "--override-input"
      "nixpkgs"
      "github:NixOS/nixpkgs/master"
    ];

    "B." = [ "nix" "build" "--file" "." ];
    "B.j" = [ "nix" "build" "--file" "." "--json" "--no-link" ];
    "R." = [ "nix" "run" "--file" "." ];
    "S." = [ "nix" "shell" "--file" "." ];
    "E." = [ "nix" "eval" "--file" "." ];
    "E.j" = [ "nix" "eval" "--file" "." "--json" ];
    "SE." = [ "nix" "search" "--file" "." ];

    Bp = [ "nix" "build" "--no-link" "--print-out-paths" "-L" "--quiet" ];

    # git
    g = [ "git" ];
    gcl = [ "git" "clone" ];
    gcl1 = [ "git" "clone" "--depth=1" ];
    gf = [ "git" "fetch" ];
    gfa = [ "git" "fetch" "--all" ];
    gfp = [ "git" "fetch" "--prune" ];
    gt = [ "git" "tag" ];
    gtl = [ "git" "tag" "--list" ];
    gts = [ "git" "tags" ];
    gp = [ "git" "push" ];
    gpf = [ "git" "push" "--force" ];
    gpl = [ "git" "pull" ];

    # sqlite
    q = [ "sqlite3" ];
    qj = [ "sqlite3" "-json" ];
    qt = [ "sqlite3" "-table" ];
    qb = [ "sqlite3" "-box" ];
    qh = [ "sqlite3" "-html" ];
    qc = [ "sqlite3" "-csv" ];

    # zig
    z = [ "zig" ];
    zb = [ "zig" "build" ];
    zbs = [ "zig" "build" "-O" "ReleaseSmall" ];
    zbe = [ "zig" "build-exe" ];
    zbes = [ "zig" "build-exe" "-O" "ReleaseSmall" ];

    # docker
    D = [ "docker" ];
    De = [ "docker" "exec" ];
    Deit = [ "docker" "exec" "-it" ];
    Dr = [ "docker" "run" ];
    Dc = [ "docker" "container" ];
    Dcl = [ "docker" "container" "ls" ];
    Di = [ "docker" "image" ];
    Dil = [ "docker" "image" "ls" ];
    Dv = [ "docker" "volume" ];
    Dvl = [ "docker" "volume" "ls" ];
    Dn = [ "docker" "network" ];
    Dnl = [ "docker" "network" "ls" ];

    # # kubernetes
    k = [ "kubectl" ];
    kg = [ "kubectl" "get" ];
    kgp = [ "kubectl" "get" "pod" ];
    kgd = [ "kubectl" "get" "deployment" ];
    kgn = [ "kubectl" "get" "node" ];
    kgpw = [ "kubectl" "get" "pod" "--watch" ];
    kgdw = [ "kubectl" "get" "deployment" "--watch" ];
    kgnw = [ "kubectl" "get" "node" "--watch" ];
    kgsw = [ "kubectl" "get" "service" ];
    kd = [ "kubectl" "describe" ];
    kdp = [ "kubectl" "describe" "pod" ];
    kdd = [ "kubectl" "describe" "deployment" ];
    kdn = [ "kubectl" "describe" "node" ];
    kc = [ "kubectl" "create" ];
    kcp = [ "kubectl" "create" "pod" ];
    kcd = [ "kubectl" "create" "deployment" ];
    kcj = [ "kubectl" "create" "job" ];
    kcn = [ "kubectl" "create" "namespace" ];
    ke = [ "kubectl" "exec" ];
    keti = [ "kubectl" "exec" "-it" ];
    kl = [ "kubectl" "label" ];
    kw = [ "kubectl" "wait" ];
    kr = [ "kubectl" "run" ];

    # npm
    nPb = [ "npm" "run" "build" ];
    nPt = [ "npm" "run" "test" ];
    nPs = [ "npm" "run" "start" ];
    nPi = [ "npm" "install" ];
    nPci = [ "npm" "ci" ];
    nPu = [ "npm" "update" ];

    # argocd
    A = [ "argocd" ];
    Aa = [ "argocd" "app" ];
    Aal = [ "argocd" "app" "list" ];
    Ac = [ "argocd" "cluster" ] ;

    # restic
    rE = [ "restic" ];
    rEb = [ "restic" "backup" ];
    rEs = [ "restic" "snapshots" ];
    rEsj = [ "restic" "snapshots" "--json" ];

    # systemctl
    sc = [ "systemctl" ];
    scc = [ "systemctl" "cat" ];
    scs = [ "systemctl" "status" ];
    sca = [ "systemctl" "start" ];
    sco = [ "systemctl" "stop" ];
    scr = [ "systemctl" "stop" ];
    sclt = [ "systemctl" "list-timers" ];
    scls = [ "systemctl" "list-sockets" ];
    scltj = [ "systemctl" "list-timers" "--output=json" ];
    sclsj = [ "systemctl" "list-sockets" "--output=json" ];
    jF = [ "journalctl" "-f" ];
    JJf = [ "journalctl" "-f" ];
    bcj = [ "busctl" "--json" ];
    bcc = [ "busctl" "call" ];
    bci = [ "busctl" "introspect" ];
    bcu = [ "busctl" "--user" ];
    bcuj = [ "busctl" "--user" "--json" ];
    bcuc = [ "busctl" "--user" "call" ];
    bcui = [ "busctl" "--user" "introspect" ];

    # zed
    # zqz = [ "zq" "-z" ];
    # zqZ = [ "zq" "-Z" ];
    # zqj = [ "zq" "-j" ];
    # zqn = [ "zq" "-f" "zng" ];
    # zqs = [ "zq" "-f" "zjson" ];

    # cargo
    # may also be done via aliases
    # https://doc.rust-lang.org/cargo/reference/config.html#alias
    C = [ "cargo" ];
    Cr = [ "cargo" "run" ];
    Crr = [ "cargo" "run" "--release" ];
    Cb = [ "cargo" "build" ];
    Cbr = [ "cargo" "build" "--release" ];
    Ct = [ "cargo" "test" ];
    Ctr = [ "cargo" "test" "--release" ];

    # misc
    j = [ "jq" "--monochrome-output" "--sort-keys" ];
    jr = [ "jq" "--monochrome-output" "--sort-keys" "--raw-output" ];
    js = [ "jq" "--slurp" ];
    jl = [ "jq" "length" ];

    jqM = [ "jq" "--monochrome-output" ];
    jcP = [ "jc" "--pretty" ];
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
    ipa = [ "ip" "a" ];
    hex = [ "hexdump" "--no-squeezing" "--canonical" ];
    hexn = [ "hexdump" "--no-squeezing" "--canonical" "--length" ];
    hexx = [ "hexyl" "--border" "none" ];
    sha1 = [ "sha1sum" ];
    sha256 = [ "sha256sum" ];
    sha2 = [ "sha256sum" ];
    sha512 = [ "sha512sum" ];
    sha5 = [ "sha512sum" ];
    grepi = [ "grep" "-i" ];
    wcc = [ "wc" "-c" ];
    wcl = [ "wc" "-l" ];
    dush = [ "du" "-sh" ];
    dfh = [ "df" "-h" ];
    dfhh = [ "df" "-h" "/home" ];
    w1 = [ "watch" "--interval" "1" ];
    w05 = [ "watch" "--interval" "0.5" ];
    mask = [ "openssl" "env" "-e" "-aes-256-ctr" "-nopad" "-nosalt" "-k" "" ];

    ungron = [ "gron" "--ungron" ];

    fd1 = [ "fd" "-j1" ];
    fd1f = [ "fd" "-j1" "-tf" ];
    fd1d = [ "fd" "-j1" "-td" ];
    fd2 = [ "fd" "-j2" ];
    fd2f = [ "fd" "-j2" "-tf" ];
    fd2d = [ "fd" "-j2" "-td" ];
    fdf = [ "fd" "-tf" ];
    fdd = [ "fd" "-td" ];

    tf = [ "terraform" ];
    tfp = [ "terraform" "plan" ];
    tfa = [ "terraform" "apply" ];
    tfs = [ "terraform" "show" ];
  };
in
{
  options = {
    nagy.shortcommands = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.str);
      default = { };
      description = "shortcommands";
    };
  };

  config = {
    environment.systemPackages =
      lib.mapAttrsToList pkgs.nur.repos.nagy.lib.mkShortCommand
        (defaultShortcommands // cfg);
  };
}
