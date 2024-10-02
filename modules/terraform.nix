{ pkgs, ... }:

{
  imports = [ ./shortcommands.nix ];

  environment.systemPackages = [
    (pkgs.opentofu.withPlugins (p: [
      p.aws
      p.github
      p.gitlab
      p.vultr
      p.kubernetes
      # p.backblaze
    ]))
    # pkgs.terraform-ls
  ];

  # https://developer.hashicorp.com/terraform/cli/commands
  # may not be needed with opentofu anymore
  environment.variables.CHECKPOINT_DISABLE = "1";

  nagy.shortcommands = {
    # tf = [ "terraform" ];
    tfp = [
      "tofu"
      "plan"
    ];
    tfa = [
      "tofu"
      "apply"
    ];
    tfs = [
      "tofu"
      "show"
    ];
    tfo = [
      "tofu"
      "output"
    ];
  };

}
