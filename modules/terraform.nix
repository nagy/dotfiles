{ pkgs, ... }:

{
  imports = [ ./shortcommands.nix ];

  environment.systemPackages = [
    (pkgs.terraform.withPlugins (p: [
      p.aws
      p.github
      p.gitlab
      p.vultr
      p.kubernetes
      # p.backblaze
    ]))
    pkgs.terraform-ls
  ];

  # https://developer.hashicorp.com/terraform/cli/commands
  environment.variables.CHECKPOINT_DISABLE = "1";

  nagy.shortcommands = {
    tf = [ "terraform" ];
    tfp = [
      "terraform"
      "plan"
    ];
    tfa = [
      "terraform"
      "apply"
    ];
    tfs = [
      "terraform"
      "show"
    ];
    tfo = [
      "terraform"
      "output"
    ];
  };

}
