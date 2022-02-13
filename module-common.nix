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

  environment.variables = {
    # cleaner git repos without the hooks
    GIT_TEMPLATE_DIR = "/var/empty";
  };

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

  environment.systemPackages = with pkgs; [ vim htop git ncdu ];

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings = {
      sandbox = true;
      trusted-users = [ "root" "@wheel" ];
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
