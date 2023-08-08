{ pkgs, lib, ... }:

{

  users.users.user = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZNW8uX6gKASOT+0XXKF2QmeXqMZfoEMIYFogbUF4jo"
    ];
  };
  # users.mutableUsers = false; # this can break the manually set password !!!!

  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "yes";
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZNW8uX6gKASOT+0XXKF2QmeXqMZfoEMIYFogbUF4jo"
  ];

  boot.kernel.sysctl = {
    # disable coredumps
    # https://wiki.archlinux.org/index.php/Core_dump#Disabling_automatic_core_dumps
    "kernel.core_pattern" = "|/bin/false";
    # This allows a special scape key: alt+print+<key>
    # https://www.kernel.org/doc/html/latest/admin-guide/sysrq.html
    "kernel.sysrq" = 1;
  };

  # all hosts should have this timezone
  time.timeZone = "Europe/Berlin";

  programs.fuse.userAllowOther = true;
  documentation.dev.enable = true;
  documentation.info.enable = true;
  networking.useDHCP = false;

  environment.localBinInPath = true;
  environment.homeBinInPath = true;

  environment.shellAliases = {
    mv = "mv --no-clobber";
    smv = "mv --no-clobber";
    # If the last character of the alias value is a blank, then the next command
    # word following the alias is also checked for alias expansion.
    # https://www.gnu.org/software/bash/manual/bash.html#Aliases
    # https://news.ycombinator.com/item?id=25243730
    sudo = "sudo ";

    to32 = "nix-hash --to-base32 --type sha256";
    lt = "ls --human-readable --size -1 -S --classify";
    ll = "ls --human-readable -l";
    la = "ls --human-readable --all -l";
    path = "echo -e \${PATH//:/\\n}";
    fastping = "ping -c 20 -i.2";
  };

  networking.hosts = {
    "1.1.1.1" = [ "1dot1dot1dot1.cloudflare-dns.com" "one.one.one.one" ];
    "8.8.8.8" = [ "dns.google" ];
    "194.242.2.2" = [ "doh.mullvad.net" ];
    "194.242.2.3" = [ "adblock.doh.mullvad.net" ];
  };

  # TODO pull from NUR module
  services.openssh.knownHosts = {
    "github.com".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
    "gist.github.com".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
    "gitlab.com".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAfuCHKVTjquxvt6CM6tdG4SLp1Btn/nOeHHE5UOzRdf";
    "git.sr.ht".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZvRd4EtM7R+IHVMWmDkVU3VLQTSwQDSAvW0t2Tkj60";
    "sr.ht".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMk9TEtn9KVMpxspbmvuAmVZ5xZD3w4Y6l6RfMFTFqiE";
    "aur.archlinux.org".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEuBKrPzbawxA/k2g6NcyV5jmqwJ2s+zpgZGZ7tpLIcN";
    "gitlab.freedesktop.org".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOzdNH/aTnOOINO/iGupQ/rYnmKF40ESCrkRg+5JkLVN";
    "codeberg.org".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIVIC02vnjFyL+I4RHfvIGNtOgJMe769VTF1VR4EB3ZB";
  };

  zramSwap = {
    enable = true;
    # memoryMax = 16 * 1024 * 1024 * 1024;
    memoryPercent = 100;
  };

  programs.ssh.extraConfig = ''
    Host *
      Protocol 2
    # Git remote hosts
    Host github.com gitlab.com git.sr.ht aur.archlinux.org gitlab.freedesktop.org codeberg.org
      User git
      RequestTTY no
  '';

  # tmpfs on all machines
  boot.tmp.useTmpfs = true;
  boot.tmp.tmpfsSize = "100%";

  # cleaner git repos without the hooks
  environment.variables.GIT_TEMPLATE_DIR = pkgs.emptyDirectory.outPath;

  console.keyMap = "de";

  # https://github.com/denoland/deno/blob/21065797f6dce285e55705007f54abe2bafb611c/cli/tools/upgrade.rs#L184-L187
  environment.variables.DENO_NO_UPDATE_CHECK = "1";

  programs.neovim = {
    enable = true;
    vimAlias = true;
    defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
    # git # already in module
    # home-manager
    jq
    fx
    tig
    yq-go
    hcl2json
    socat
    jo
    jc
    taplo
    htmlq

    # network
    dstat
    nftables
    sshfs-fuse

    # processes
    # ltrace # not available on aarch64
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
    sqlite
    oil

    black
    isort
    cryptsetup

    # documentation
    man-pages
    glibcInfo # info files for gnu glibc

    # custom tooling
    # (pkgs.writeScriptBin "journal-git-store"
    #   (builtins.readFile ../bin/journal-git-store))
    (pkgs.writeScriptBin "gitpack" (builtins.readFile ../bin/gitpack))
    nixfmt
    nil
    nickel
    topiary
    dnsutils
    yt-dlp
    nix-update
    nix-prefetch
    nix-prefetch-git
    nix-diff
    nix-init
    qrencode
    restic
    rclone
    zig
    wasmtime
    (zbar.override {
      withXorg = false;
      enableVideo = false;
    })
    typos
    shellcheck
    (aspellWithDicts (ps: [ ps.en ]))
    (lispPackages_new.sbclWithPackages (ps:
      with ps; [
        (slynk.overrideLispAttrs ({ systems, ... }: {
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
        }))
        april
        serapeum
        dbus
      ]))

    (pass.withExtensions (exts: [ exts.pass-otp ]))
    pinentry
    (gnupg.override { guiSupport = false; })
    xurls
    (hy.withPackages (ps: with ps; [ hyrule addict ]))
    (terraform.withPlugins (p: with p; [ github ]))
    pkgs.nur.repos.nagy.hyperspec
    (pkgs.nur.repos.nagy.lib.mkRustScript { file = ../bin/blocker.rs; })
    # version control
    gh
    hut
    tea
    gron
  ];

  boot.binfmt.emulatedSystems =
    [ "wasm32-wasi" "aarch64-linux" "armv6l-linux" ];

  boot.binfmt.registrations.oil = {
    recognitionType = "extension";
    magicOrExtension = "oil";
    interpreter = lib.getExe pkgs.oil;
  };

  # not fulfilled by above "wasm32-wasi"
  boot.binfmt.registrations.wat = {
    recognitionType = "extension";
    magicOrExtension = "wat";
    interpreter = lib.getExe pkgs.wasmtime;
  };

  # boot.binfmt.registrations.gba = {
  #   recognitionType = "extension";
  #   magicOrExtension = "gba";
  #   interpreter = getExe pkgs.mgba;
  # };

  environment.variables.LESSHISTFILE = "-";

}
