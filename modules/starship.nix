{ pkgs, lib, ... }:

{
  # simpler version of starship
  # until https://github.com/starship/starship/issues/896 is fixed
  environment.variables.STARSHIP_CONFIG =
    let
      mkDollarPrompt =
        lib.replaceStrings [ ">](bold green)" ] [ "\\\\$](bold green)" ];
      basePreset = builtins.readFile
        "${pkgs.starship.src}/docs/.vuepress/public/presets/toml/plain-text-symbols.toml";
      basePresetModified = ''
        add_newline=false
      '' + (mkDollarPrompt basePreset);
    in
    toString (pkgs.writeText "starship-config.toml" basePresetModified);
  programs.bash.interactiveShellInit = ''
    if [[ $TERM != "dumb" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "29.1,eat") ]]; then
      export STARSHIP_CACHE=/run/user/$UID/starship-cache
      eval "$(${pkgs.starship}/bin/starship init bash --print-full-init)"
    fi
    [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
      source "$EAT_SHELL_INTEGRATION_DIR/bash"
    HISTCONTROL=ignoredups:ignorespace
    HISTFILESIZE=10000000
    HISTSIZE=1000000
    HISTFILE="$HOME/.local/share/bash_history"
  '';
  # the starship binary could also be added to system packages. This is needed
  # when using prompt explanations
}
