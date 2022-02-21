{ pkgs, ... }:

{

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
  };

  # all hosts should have this timezone
  time.timeZone = "Europe/Berlin";

  programs.fuse.userAllowOther = true;
  documentation.dev.enable = true;
  documentation.info.enable = true;
  # documentation.nixos.enable = true; alreay the default
  networking.firewall.enable = false;

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
    };
  };
  environment.variables.HTOPRC = "/etc/htoprc";

  programs.git = {
    enable = true;
    config = {
      aliases = {
        c = "commit";
        co = "checkout";
        cl = "clone";
        cl1 = "clone --depth 1";
        f = "fetch";
        lol = "log --graph --decorate --pretty=oneline --abbrev-commit";
        lola = "lol --all";
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
  };

  programs.tmux = {
    enable = true;
    baseIndex = 1;
    keyMode = "vi";
    extraConfig = "bind -n C-k clear-history";
  };

  services.openssh.knownHosts = {
    "github.com".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
    "gitlab.com".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAfuCHKVTjquxvt6CM6tdG4SLp1Btn/nOeHHE5UOzRdf";
    "git.sr.ht".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZvRd4EtM7R+IHVMWmDkVU3VLQTSwQDSAvW0t2Tkj60";
    "sr.ht".publicKey =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMk9TEtn9KVMpxspbmvuAmVZ5xZD3w4Y6l6RfMFTFqiE";
  };

  zramSwap = {
    enable = true;
    # memoryMax = 16 * 1024 * 1024 * 1024;
    memoryPercent = 100;
  };

  programs.ssh.extraConfig = ''
    # Git remote hosts
    Host github.com gitlab.com git.sr.ht
      User git
      RequestTTY no
  '';

  # tmpfs on all machines
  boot.tmpOnTmpfs = true;

  # cleaner git repos without the hooks
  environment.variables.GIT_TEMPLATE_DIR = "/var/empty";

  console = {
    font = "ter-i32b";
    keyMap = "de";
    packages = [ pkgs.terminus_font ];
    # earlySetup = true;
  };

  # https://askubuntu.com/questions/493002/global-sudo-session-in-ubuntu
  security.sudo.extraConfig = ''
    Defaults:user !tty_tickets, timestamp_timeout=60
  '';

  environment.systemPackages = with pkgs; [
    vim
    git
    home-manager
    screen

    # network
    dstat

    # processes
    # ltrace # not available on aarch64
    killall

    # files
    tree
    file
    fd
    ripgrep
    ncdu

    # documentation
    man-pages
  ];

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings = {
      sandbox = true;
      trusted-users = [ "root" "@wheel" ];
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
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
    };
  };
}
