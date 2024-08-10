{ config, lib, nur, ... }:

let
  cfg = config.nagy.shortcommands;
  defaultShortcommands = {
    # nix flakes
    n = [ "nix" ];
    b = [ "nix-build" ];
    i = [ "nix-instantiate" ];
    "b," = [ "nix-build" "<nixpkgs>" ];
    "i," = [ "nix-instantiate" "<nixpkgs>" ];
    "b,," = [ "nix-build" "<nixpkgs/nixos>" ];
    "i,," = [ "nix-instantiate" "<nixpkgs/nixos>" ];
    "b." = [ "nix-build" "<dot>" ];
    "i." = [ "nix-instantiate" "<dot>" ];
    R = [ "nix" "run" ];
    SE = [ "nix" "search" ];
    B = [ "nix" "build" ];
    E = [ "nix" "eval" ];
    F = [ "nix" "flake" ];
    P = [ "nix" "profile" ];
    S = [ "nix" "shell" ];
    BL = [ "nix" "build" "--print-build-logs" ];
    RL = [ "nix" "run" "--print-build-logs" ];
    Ej = [ "nix" "eval" "--json" ];
    Er = [ "nix" "eval" "--raw" ];
    Bj = [ "nix" "build" "--json" "--no-link" ];
    Pl = [ "nix" "profile" "list" ];
    Pi = [ "nix" "profile" "install" ];
    # Pu = [ "nix" "profile" "upgrade" ];
    # Pr = [ "nix" "profile" "remove" ];
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

    "B." = [ "nix" "build" "--file" "." ];
    "B.j" = [ "nix" "build" "--file" "." "--json" "--no-link" ];
    "R." = [ "nix" "run" "--file" "." ];
    "S." = [ "nix" "shell" "--file" "." ];
    "E." = [ "nix" "eval" "--file" "." ];
    "E.j" = [ "nix" "eval" "--file" "." "--json" ];
    "SE." = [ "nix" "search" "--file" "." ];

    "B:" = ["nix" "build" "--file" "flake:nixpkgs"];
    "B:j" = ["nix" "build" "--file" "flake:nixpkgs" "--json" "--no-link"];
    "R:" = ["nix" "run" "--file" "flake:nixpkgs"];
    "D:" = ["nix" "develop" "--file" "flake:nixpkgs"];
    "S:" = ["nix" "shell" "--file" "flake:nixpkgs"];
    "E:" = ["nix" "eval" "--file" "flake:nixpkgs"];
    "E:j" = ["nix" "eval" "--file" "flake:nixpkgs" "--json"];

    Bp = [ "nix" "build" "--no-link" "--print-out-paths" "-L" "--quiet" ];

    # sqlite
    q = [ "sqlite3" ];
    qj = [ "sqlite3" "-json" ];
    qt = [ "sqlite3" "-table" ];
    qb = [ "sqlite3" "-box" ];
    qh = [ "sqlite3" "-html" ];
    qc = [ "sqlite3" "-csv" ];

    # systemctl
    sc = [ "systemctl" ];
    scc = [ "systemctl" "cat" ];
    scs = [ "systemctl" "status" ];
    sca = [ "systemctl" "start" ];
    sco = [ "systemctl" "stop" ];
    scr = [ "systemctl" "restart" ];
    sclt = [ "systemctl" "list-timers" ];
    scls = [ "systemctl" "list-sockets" ];
    sclm = [ "systemctl" "list-machines" ];
    sclu = [ "systemctl" "list-units" ];
    sclf = [ "systemctl" "list-unit-files" ];
    scltj = [ "systemctl" "list-timers" "--output=json" ];
    sclsj = [ "systemctl" "list-sockets" "--output=json" ];
    sclmj = [ "systemctl" "list-machines" "--output=json" ];
    scluj = [ "systemctl" "list-units" "--output=json" ];
    sclfj = [ "systemctl" "list-unit-files" "--output=json" ];
    scU = [ "systemctl" "--user" ];
    scUc = [ "systemctl" "--user" "cat" ];
    scUs = [ "systemctl" "--user" "status" ];
    scUa = [ "systemctl" "--user" "start" ];
    scUo = [ "systemctl" "--user" "stop" ];
    scUr = [ "systemctl" "--user" "restart" ];
    scUlt = [ "systemctl" "--user" "list-timers" ];
    scUls = [ "systemctl" "--user" "list-sockets" ];
    scUlm = [ "systemctl" "--user" "list-machines" ];
    scUlu = [ "systemctl" "--user" "list-units" ];
    scUlf = [ "systemctl" "--user" "list-unit-files" ];
    scUltj = [ "systemctl" "--user" "list-timers" "--output=json" ];
    scUlsj = [ "systemctl" "--user" "list-sockets" "--output=json" ];
    scUlmj = [ "systemctl" "--user" "list-machines" "--output=json" ];
    scUluj = [ "systemctl" "--user" "list-units" "--output=json" ];
    scUlfj = [ "systemctl" "--user" "list-unit-files" "--output=json" ];
    JF = [ "journalctl" "-f" ];
    # JJf = [ "journalctl" "-f" ];

    # misc
    j = [ "jq" "--monochrome-output" "--sort-keys" ];
    jr = [ "jq" "--monochrome-output" "--sort-keys" "--raw-output" ];
    js = [ "jq" "--slurp" ];
    jl = [ "jq" "length" ];

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
    ipa = [ "ip" "address" ];
    ipl = [ "ip" "link" ];
    ipr = [ "ip" "route" ];
    ipn = [ "ip" "neighbour" ];
    i4a = [ "ip" "-4" "address" ];
    i4l = [ "ip" "-4" "link" ];
    i4r = [ "ip" "-4" "route" ];
    i4n = [ "ip" "-4" "neighbour" ];
    i6a = [ "ip" "-6" "address" ];
    i6l = [ "ip" "-6" "link" ];
    i6r = [ "ip" "-6" "route" ];
    i6n = [ "ip" "-6" "neighbour" ];
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
    # w1 = [ "watch" "--interval" "1" ];
    w05 = [ "watch" "--interval" "0.5" ];
    mask = [ "openssl" "env" "-e" "-aes-256-ctr" "-nopad" "-nosalt" "-k" "" ];

    ungron = [ "gron" "--ungron" ];
    pingc3 = [ "ping" "-c" "3" ];

    fd1 = [ "fd" "-j1" ];
    fd1f = [ "fd" "-j1" "-tf" ];
    fd1d = [ "fd" "-j1" "-td" ];
    fd2 = [ "fd" "-j2" ];
    fd2f = [ "fd" "-j2" "-tf" ];
    fd2d = [ "fd" "-j2" "-td" ];
    fdf = [ "fd" "-tf" ];
    fdd = [ "fd" "-td" ];

    digs = [ "dig" "+short" ];
    dig6 = [ "dig" "AAAA" ];
    dig6s = [ "dig" "AAAA" "+short" ];
    digt = [ "dig" "TXT" ];
    digts = [ "dig" "TXT" "+short" ];

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
      lib.mapAttrsToList nur.repos.nagy.lib.mkShortCommand
        (defaultShortcommands // cfg);
  };
}
