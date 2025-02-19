{
  imports = [ ./shortcommands.nix ];

  # environment.systemPackages = [ pkgs.tig ];

  programs.git = {
    enable = true;
    config = {
      user.name = "Daniel Nagy";
      user.email = "danielnagy@posteo.de";
      user.signingkey = "/home/user/.ssh/id_nagy";
      gpg = {
        format = "ssh";
        ssh.allowedSignersFile = "/home/user/.config/git/allowed_signers";
      };
      # url = {
      # forges
      # "https://github.com/".insteadOf = "gh:";
      # "https://gist.github.com/".insteadOf = "gist:";
      # "https://gitlab.com/".insteadOf = "gl:";
      # "https://git.sr.ht/".insteadOf = "sh:";
      # "https://codeberg.org/".insteadOf = "cb:";
      # "https://aur.archlinux.org/".insteadOf = "aur:";
      # "https://gitlab.archlinux.org/".insteadOf = "archlinux:";
      # "https://gitlab.alpinelinux.org/".insteadOf = "alpine:";
      # nagy repos
      # "git@github.com:nagy/".insteadOf = "ghn:";
      # "git@gitlab.com:nagy/".insteadOf = "gln:";
      # "git@git.sr.ht:~nagy/".insteadOf = "shn:";
      # "git@codeberg.org:nagy/".insteadOf = "cbn:";
      # external repos
      # "https://github.com/NixOS/nixpkgs".insteadOf = "nixpkgs:";
      # "https://github.com/nix-community/NUR".insteadOf = "NUR:";
      # "https://code.tvl.fyi/depot.git".insteadOf = "tvl:";
      # };
    };
  };

  nagy.shortcommands = {
    g = [ "git" ];
    gcl = [
      "git"
      "clone"
    ];
    gcl1 = [
      "git"
      "clone"
      "--depth=1"
    ];
    gf = [
      "git"
      "fetch"
    ];
    gfa = [
      "git"
      "fetch"
      "--all"
    ];
    gfp = [
      "git"
      "fetch"
      "--prune"
    ];
    gt = [
      "git"
      "tag"
    ];
    gtl = [
      "git"
      "tag"
      "--list"
    ];
    gts = [
      "git"
      "tags"
    ];
    gp = [
      "git"
      "push"
    ];
    gpf = [
      "git"
      "push"
      "--force"
    ];
    gpl = [
      "git"
      "pull"
    ];
  };
}
