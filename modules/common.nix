{
  config,
  pkgs,
  lib,
  nur,
  ...
}:

{
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

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      ClientAliveInterval = 60;
    };
  };

  # all hosts should have this timezone
  time.timeZone = "Europe/Berlin";
  console.keyMap = lib.mkDefault "de";

  documentation.dev.enable = true;
  documentation.info.enable = true;

  programs.ssh.extraConfig = ''
    Host *
      StrictHostKeyChecking accept-new
      ServerAliveInterval 300
      ServerAliveCountMax 2
  '';

  environment.systemPackages = with pkgs; [
    # Python
    (python3.withPackages (ps: [
      ps.hy
      ps.hyrule
      ps.addict
    ]))
    black
    isort
    ruff
    cryptsetup

    # documentation
    man-pages
    # broken currently glibcInfo # info files for gnu glibc

    # custom tooling
    mtr
    dnsutils
    rclone
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
    nur.repos.nagy.cxxmatrix
    dool
    # for man pages only
    (lib.getMan isync)
    doggo
    # (import (builtins.fetchTarball "https://github.com/nagy/jsonrpcrun/archive/master.tar.gz") {
    #   inherit pkgs;
    # })
    (import <jsonrpcrun> {
      inherit pkgs;
    })
  ];

  environment.etc."rfc" = lib.mkIf config.documentation.nixos.enable {
    source = "${nur.repos.nagy.rfcs}/share/rfc";
  };
}
