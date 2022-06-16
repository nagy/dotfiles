{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.fzf ];

  programs.bash.interactiveShellInit = ''
    if [[ :$SHELLOPTS: =~ :(vi|emacs): ]]; then
      . ${pkgs.fzf}/share/fzf/completion.bash
      . ${pkgs.fzf}/share/fzf/key-bindings.bash
    fi
  '';
}
