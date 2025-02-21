{
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
    };
  };
}
