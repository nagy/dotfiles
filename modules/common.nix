{ pkgs, ... }:

with pkgs.lib; {

  # services.getty.autologinUser = "user";

  users.users.user = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZNW8uX6gKASOT+0XXKF2QmeXqMZfoEMIYFogbUF4jo"
    ];
  };

  services.openssh.enable = true;
  services.openssh.forwardX11 = true;
  services.openssh.permitRootLogin = "yes";
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
  # documentation.nixos.enable = true; alreay the default
  networking.firewall.enable = false;
  networking.useDHCP = false;

  # simpler version of starship
  # until https://github.com/starship/starship/issues/896 is fixed
  environment.variables.STARSHIP_CONFIG = let
    mkDollarPrompt =
      pkgs.lib.replaceStrings [ ">](bold green)" ] [ "\\\\$](bold green)" ];
    basePreset = builtins.readFile
      "${pkgs.starship.src}/docs/.vuepress/public/presets/toml/plain-text-symbols.toml";
    basePresetModified = ''
      add_newline=false
    '' + (mkDollarPrompt basePreset);
  in toString (pkgs.writeText "starship-config.toml" basePresetModified);
  environment.variables.STARSHIP_CACHE = "/tmp/starship-cache";
  programs.bash.interactiveShellInit = ''
    if [[ $TERM != "dumb" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
      eval "$(${pkgs.starship}/bin/starship init bash --print-full-init)"
    fi
  '';
  # the starship binary could also be added to system packages. This is needed
  # when using prompt explanations

  programs.htop = {
    enable = true;
    settings = {
      delay = 2;
      header_margin = 0;
      hide_kernel_threads = 1;
      hide_userland_threads = 1;
      highlight_megabytes = 1;
      left_meter_modes = [ 1 1 1 2 ];
      left_meters = [ "AllCPUs2" "Memory" "Swap" "Zram" ];
      right_meter_modes = [ 2 2 2 2 ];
      right_meters = [ "Tasks" "LoadAverage" "Uptime" "Systemd" ];
      show_program_path = 0;
      tree_view = 1;
      highlight_changes = 1;
      highlight_changes_delay_secs = 2;
      update_process_names = 1;
      # manually removed "nice" and "prio" column
      fields = "0 48 38 39 40 2 46 47 49 1";
    };
  };
  environment.variables.HTOPRC = "/etc/htoprc";
  environment.localBinInPath = true;
  environment.homeBinInPath = true;

  programs.git = {
    enable = true;
    config = {
      alias = {
        # c = "commit"; # in included git aliases
        # co = "checkout"; # in included git aliases
        cl = "clone";
        cl1 = "clone --depth 1";
        f = "fetch";
        lol = "log --graph --decorate --pretty=oneline --abbrev-commit";
        lola = "lol --all";
      };
      user.name = "Daniel Nagy";
      user.email = "danielnagy@posteo.de";
      commit = {
        # Show my changes when writing the message
        verbose = true;
      };
      init = { defaultBranch = "master"; };
      push = { default = "current"; };
      pull.rebase = true;
      include.path = let
        git-alias = pkgs.fetchFromGitHub {
          owner = "GitAlias";
          repo = "gitalias";
          rev = "a0bd5343f4dcc6e11ee9f5e04c36ebd1166eb4c0";
          sha256 = "17fgg2znhl2n0vw1ym9dnjvm3sgj5ynzr1r9najw2rw3f07y8wd7";
        };
      in "${git-alias}/gitalias.txt";
      merge.conflictStyle = "diff3";
      gc = { auto = "0"; };
      url = {
        "https://github.com/".insteadOf = "gh:";
        "https://gist.github.com/".insteadOf = "gist:";
        "https://gitlab.com/".insteadOf = "gl:";
        "https://aur.archlinux.org/".insteadOf = "aur:";
        "https://git.sr.ht/".insteadOf = "srht:";
        "https://bitbucket.org/".insteadOf = "bb:";
        "git@github.com:nagy/".insteadOf = "ghn:";
      };
      tar = {
        "tar.xz".command = "${pkgs.xz}/bin/xz -c";
        "tar.bz2".command = "${pkgs.bzip2}/bin/bzip2 -c";
        "tar.zst".command = "${pkgs.zstd}/bin/zstd -c";
      };
      # Shiny colors
      color = {
        branch = "auto";
        diff = "auto";
        interactive = "auto";
        status = "auto";
        ui = "auto";
      };

      # Pretty much the usual diff colors
      "color.diff" = {
        commit = "yellow";
        frag = "cyan";
        meta = "yellow";
        new = "green";
        old = "red";
        whitespace = "red reverse";
      };

      "color.diff-highlight" = {
        oldNormal = "red bold";
        oldHighlight = "red bold 52";
        newNormal = "green bold";
        newHighlight = "green bold 22";
      };
      # To work around the workaround of CVE-2022-24765.
      # See https://github.com/NixOS/nixpkgs/issues/169193 for more
      safe.directory = "*";
      filter = {
        # use with `.gitattributes` file content: *.sqlite3 filter=sqlite3-sql
        # more info https://github.com/theTaikun/SQLite-git-smudge-and-clean
        sqlite3-sql = {
          clean = "${pkgs.sqlite}/bin/sqlite3 %f .dump";
          smudge = toString (pkgs.writeShellScript "git-smudge-sqlite3" ''
            TMPFILE=$(mktemp)
            cat | ${pkgs.sqlite}/bin/sqlite3 "$TMPFILE"
            cat -- "$TMPFILE"
            rm -f -- "$TMPFILE"
          '');
        };
      };
    };
  };

  environment.shellAliases = {
    t = "tmux";
    h = "htop";
    g = "git";
    hm = "home-manager";
    mv = "mv --no-clobber";
    smv = "mv --no-clobber";
    # If the last character of the alias value is a blank, then the next command
    # word following the alias is also checked for alias expansion.
    # https://www.gnu.org/software/bash/manual/bash.html#Aliases
    # https://news.ycombinator.com/item?id=25243730
    sudo = "sudo ";

    # from jonringer
    to32 = "nix-hash --to-base32 --type sha256";
    nfl = "nix flake lock";
    nflu = "nix flake lock --update-input";
    # ns="nix-shell"; # eventually switch to `nix develop`
    gco = "git checkout";
    gst = "git status";

    lt = "ls --human-readable --size -1 -S --classify";
    ll = "ls --human-readable -l";
    la = "ls --human-readable --all -l";
    path = "echo -e \${PATH//:/\\n}";
    fastping = "ping -c 20 -i.2";
  };

  programs.tmux = {
    enable = true;
    baseIndex = 1;
    keyMode = "vi";
    extraConfig = "bind C-k clear-history";
  };

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
  };

  zramSwap = {
    enable = true;
    # memoryMax = 16 * 1024 * 1024 * 1024;
    memoryPercent = 100;
  };

  programs.ssh.extraConfig = ''
    # Git remote hosts
    Host github.com gitlab.com git.sr.ht aur.archlinux.org gitlab.freedesktop.org
      User git
      RequestTTY no
  '';

  # tmpfs on all machines
  boot.tmpOnTmpfs = true;

  # cleaner git repos without the hooks
  environment.variables.GIT_TEMPLATE_DIR = pkgs.emptyDirectory.outPath;

  console.keyMap = "de";

  # https://askubuntu.com/questions/493002/global-sudo-session-in-ubuntu
  security.sudo.extraConfig = ''
    Defaults:user !tty_tickets, timestamp_timeout=60
  '';

  programs.neovim = {
    enable = true;
    vimAlias = true;
    defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
    # git # already in module
    home-manager
    jq
    tig
    yq-go
    hcl2json
    socat
    jo
    jc
    # comma # somehow not loaded with its db
    rustc
    cargo
    clippy

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
    jpegoptim
    optipng
    uhubctl
    usbutils
    sqlite
    oil

    black
    isort
    cryptsetup

    # ncdu_1
    # need to rebuild because of broken zig
    (ncdu_2.overrideAttrs (old: { pname = old.pname + "-rebuild"; }))

    # documentation
    man-pages
    glibcInfo # info files for gnu glibc

    # custom tooling
    # (pkgs.writeScriptBin "journal-git-store"
    #   (builtins.readFile ../bin/journal-git-store))
    (pkgs.writeScriptBin "gitpack" (builtins.readFile ../bin/gitpack))
    nixfmt
    wordnet
    yt-dlp
    nix-update
    nix-prefetch
    nix-prefetch-git
    qrencode
    restic
    rclone
    bup
    (zbar.override { enableVideo = false; })
    shellcheck
    (aspellWithDicts (ps: [ ps.en ]))
    (lispPackages_new.sbclWithPackages (ps:
      with ps; [
        (slynk.overrideLispAttrs (o: {
          systems = o.systems ++ [
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

    (pkgs.pass.withExtensions (exts: [ exts.pass-otp ]))
    pinentry
    (gnupg.override { guiSupport = false; })
    gh

    (hy.withPackages (ps: with ps; [ hyrule addict ]))
    (terraform.withPlugins (p: with p; [ github vultr ]))
    pyright
    qemu

    fennel
    fnlfmt

    nix-doc
    zed
  ];

  # boot.binfmt.emulatedSystems = [
  #   "wasm32-wasi"
  # ];

  boot.binfmt.registrations.oil = {
    recognitionType = "extension";
    magicOrExtension = "oil";
    interpreter = pkgs.lib.getExe pkgs.oil;
  };

  environment.variables.LESSHISTFILE = "-";

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes recursive-nix
      warn-dirty = false
      plugin-files = ${pkgs.nix-doc}/lib/libnix_doc_plugin.so
    '';
    gc = {
      automatic = true;
      dates = "monthly";
      options = "--delete-older-than 14d";
    };
    settings = {
      sandbox = true;
      trusted-users = [ "root" "@wheel" ];
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
    nixPath = mkOptionDefault [ "dot=${../.}" ];
    registry = {
      nagy.to = {
        owner = "nagy";
        repo = "nur-packages";
        type = "github";
      };
      dot.to = {
        owner = "nagy";
        repo = "dotfiles";
        type = "github";
      };
      N.to = {
        id = "nagy";
        type = "indirect";
      };
      n.to = {
        id = "nixpkgs";
        type = "indirect";
      };
      pkgs.to = {
        id = "nixpkgs";
        type = "indirect";
      };
      HW.to = {
        id = "nixos-hardware";
        type = "indirect";
      };
      u.to = {
        owner = "NixOS";
        repo = "nixpkgs";
        type = "github";
        ref = "nixos-unstable";
      };
      G.to = {
        id = "gemini";
        type = "indirect";
      };
      ncl.to = {
        id = "nickel";
        type = "indirect";
      };
      hm.to = {
        id = "home-manager";
        type = "indirect";
      };
      HM.to = {
        id = "home-manager";
        type = "indirect";
      };
      json2dbus.to = {
        owner = "nagy";
        repo = "json2dbus";
        type = "github";
      };
      NG.to = {
        id = "nixos-generators";
        type = "indirect";
      };
      # not yet a PR
      comma.to = {
        owner = "nix-community";
        repo = "comma";
        type = "github";
      };
      nixt.to = {
        owner = "nix-community";
        repo = "nixt";
        type = "github";
      };
      # until https://github.com/NixOS/flake-registry/pull/33
      nixos-generators.to = {
        owner = "nix-community";
        repo = "nixos-generators";
        type = "github";
      };
      j2d.to = {
        owner = "nagy";
        repo = "json2dbus";
        type = "github";
      };
      nixos-shell.to = {
        owner = "Mic92";
        repo = "nixos-shell";
        type = "github";
      };
      eo.to = {
        id = "emacs-overlay";
        type = "indirect";
      };
      crystal2nix.to = {
        owner = "nix-community";
        repo = "crystal2nix";
        type = "github";
      };
      nix-doom-emacs.to = {
        owner = "nix-community";
        repo = "nix-doom-emacs";
        type = "github";
      };
      nix-mode.to = {
        owner = "NixOS";
        repo = "nix-mode";
        type = "github";
      };
      blobber.to = {
        owner = "nagy";
        repo = "blobber";
        type = "github";
      };
      microvm.to = {
        owner = "astro";
        repo = "microvm.nix";
        type = "github";
      };
    };
  };
}
