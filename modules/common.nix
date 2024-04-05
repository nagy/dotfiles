{ pkgs, lib, ... }:

{

  users.users.user = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "dialout" ];
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
    llH = "ls --human-readable -l --dereference-command-line";
    laH = "ls --human-readable --all -l --dereference-command-line";
    ltH = "ls --human-readable --size -1 -S --classify --dereference-command-line";
    path = "echo -e \${PATH//:/\\\\n}";
    nixpath = "echo -e \${NIX_PATH//:/\\\\n}";
    fastping = "ping -c 20 -i.2";
    reset = "tput reset";
    ".." = "cd ..";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    "....." = "cd ../../../..";
    "......" = "cd ../../../../..";
    "......." = "cd ../../../../../..";
  };

  networking.hosts = {
    "1.1.1.1" = [
      "1dot1dot1dot1.cloudflare-dns.com"
      "one.one.one.one"
    ];
    "8.8.8.8" = [ "dns.google" ];
    # https://mullvad.net/de/help/dns-over-https-and-dns-over-tls
    "194.242.2.2" = [ "dns.mullvad.net" ];
    "194.242.2.3" = [ "adblock.dns.mullvad.net" ];
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
    Host github.com ssh.github.com gitlab.com git.sr.ht aur.archlinux.org codeberg.org gitlab.*
      User git
      RequestTTY no
    Host ssh.github.com
      Port 443
  '';

  # tmpfs on all machines
  boot.tmp.useTmpfs = true;
  boot.tmp.tmpfsSize = "100%";

  # cleaner git repos without the hooks
  environment.variables.GIT_TEMPLATE_DIR = pkgs.emptyDirectory.outPath;

  console.keyMap = "de";

  # https://github.com/denoland/deno/blob/21065797f6dce285e55705007f54abe2bafb611c/cli/tools/upgrade.rs#L184-L187
  environment.variables.DENO_NO_UPDATE_CHECK = "1";

  # https://developer.hashicorp.com/terraform/cli/commands
  environment.variables.CHECKPOINT_DISABLE = "1";

  programs.neovim = {
    enable = true;
    vimAlias = true;
    defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
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
    oil
    optipng

    black
    isort
    cryptsetup

    # documentation
    man-pages
    glibcInfo # info files for gnu glibc

    # custom tooling
    # (pkgs.writeScriptBin "journal-git-store"
    #   (builtins.readFile ../bin/journal-git-store))
    topiary
    dnsutils
    yt-dlp
    qrencode
    restic
    rclone
    zig
    zls
    wabt
    wasmtime
    (zbar.override {
      withXorg = false;
      enableVideo = false;
    })
    typos
    shellcheck
    (aspellWithDicts (ps: [ ps.en ps.de]))
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
      ]
    ))

    (pass.withExtensions (exts: [ exts.pass-otp ]))
    pinentry
    (gnupg.override { guiSupport = false; })
    xurls
    (hy.withPackages (ps: with ps; [ hyrule addict ]))
    (opentofu.withPlugins (p: with p; [ github vultr ]))
    pkgs.nur.repos.nagy.hyperspec
    # version control
    gh
    hut
    tea
    gron
    ruff
    dool
    (
      # malloc-trim.sh
      # From http://notes.secretsauce.net/notes/2016/04/08_glibc-malloc-inefficiency.html
      pkgs.writeShellScriptBin "malloc-trim" ''
        set -e
        PID=$1
        test -n "$PID" || { echo "Need PID on the cmdline" > /dev/stderr; exit 1; }

        before=`ps -h -p $PID -O rss  | awk '{print $2}'`
        gdb --batch-silent --eval-command 'call (int)malloc_trim(0)' -p $PID
        after=`ps -h -p $PID -O rss  | awk '{print $2}'`

        echo "before: $before"
        echo "after: $after"
        echo "freed: $(($before - $after))"
      ''
    )
  ];

  boot.binfmt.emulatedSystems = [
    "wasm32-wasi"
    # "aarch64-linux" # remove, because it causes any raspberry pi systems to become unbootable
    "armv6l-linux"
  ];

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
