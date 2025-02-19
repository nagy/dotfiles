{
  config,
  pkgs,
  lib,
  nur,
  ...
}:

{
  # This prevents all ipv6 comm on all interfaces
  # boot.kernel.sysctl."net.ipv6.conf.lo.disable_ipv6" = true;

  users.users.user = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [
      "wheel"
      "dialout"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZNW8uX6gKASOT+0XXKF2QmeXqMZfoEMIYFogbUF4jo"
    ];
  };
  # users.mutableUsers = false; # this can break the manually set password !!!!

  users.extraUsers.root.openssh.authorizedKeys.keys =
    config.users.users.user.openssh.authorizedKeys.keys;
  programs.ssh = {
    enableAskPassword = lib.mkForce false;
  };

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      ClientAliveInterval = 60;
    };
  };

  boot.kernel.sysctl = {
    # disable coredumps
    # https://wiki.archlinux.org/index.php/Core_dump#Disabling_automatic_core_dumps
    "kernel.core_pattern" = "|/bin/false";
  };

  # all hosts should have this timezone
  time.timeZone = "Europe/Berlin";

  documentation.dev.enable = true;
  documentation.info.enable = true;
  # networking.useDHCP = false;
  # networking.dhcpcd.enable = false;

  programs.ssh.extraConfig = ''
    Host *
      StrictHostKeyChecking accept-new
      ServerAliveInterval 300
      ServerAliveCountMax 2
  '';

  console.keyMap = lib.mkDefault "de";

  environment.systemPackages = with pkgs; [
    jq
    fx
    yq-go
    hcl2json
    socat
    jo
    jc
    taplo
    htmlq
    yj

    ## Network
    nftables

    ## Processes
    killall
    bubblewrap

    # files
    tree
    file
    fd
    ripgrep
    lsof
    tokei
    unzip
    usbutils
    sqlite-interactive
    optipng
    pngquant
    jpegoptim
    brotli
    vultr-cli

    black
    isort
    cryptsetup
    pv
    # unrar-free

    # documentation
    man-pages
    # broken currently glibcInfo # info files for gnu glibc

    # custom tooling
    topiary
    dnsutils
    qrencode
    rclone
    typos
    shellcheck
    (aspellWithDicts (ps: [
      ps.en
      ps.de
    ]))
    # for jinx-mode to set DICPATH
    hunspellDicts.en-us
    hunspellDicts.de-de
    # (nuspellWithDicts [
    # hunspellDicts.en-us
    # hunspellDicts.de-de
    # ])
    (sbcl.withPackages (
      ps: with ps; [
        (slynk.overrideLispAttrs (
          { systems, ... }:
          {
            systems = systems ++ [
              "slynk/mrepl"
              "slynk/indentation"
              "slynk/stickers"
              "slynk/trace-dialog"
              "slynk/package-fu"
              "slynk/fancy-inspector"
              "slynk/arglists"
              "slynk/profiler"
              "slynk/retro"
            ];
          }
        ))
        # april
        serapeum
      ]
    ))

    # xurls
    (python3.withPackages (ps: [
      ps.hy
      ps.hyrule
      ps.addict
    ]))
    k9s
    nur.repos.nagy.hyperspec
    nur.repos.nagy.cxxmatrix
    # version control
    gh
    # hut
    # tea
    gron
    ruff
    dool
    universal-ctags
    # for man pages only
    (lib.getMan isync)
    doggo
    # (import (builtins.fetchTarball "https://github.com/nagy/jsonrpcrun/archive/master.tar.gz") {
    #   inherit pkgs;
    # })
    (import <jsonrpcrun> {
      inherit pkgs;
    })
    mtr
  ];

  environment.etc."rfc" = lib.mkIf config.documentation.nixos.enable {
    source = "${nur.repos.nagy.rfcs}/share/rfc";
  };

  # not used anywhere, might save some space.
  boot.supportedFilesystems.zfs = lib.mkForce false;
}
