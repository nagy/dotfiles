{ pkgs, lib, ... }:

let
  starship_patched = pkgs.starship.overrideAttrs (
    {
      patches ? [ ],
      ...
    }:
    {
      patches = patches ++ [
        (pkgs.fetchpatch {
          url = "https://github.com/starship/starship/pull/5899.patch";
          hash = "sha256-/XTwSMgK9KX/RbVHs6ccLCnSCbndp21aHVOWT//wDgc=";
        })
      ];
    }
  );
in
{
  # simpler version of starship
  # until https://github.com/starship/starship/issues/896 is fixed
  # FIXME rework this for 24.05. presets are now part of the module
  environment.variables.STARSHIP_CONFIG =
    let
      mkDollarPrompt = lib.replaceStrings [ ">](bold green)" ] [ "\\\\$](bold green)" ];
      basePreset = builtins.readFile "${starship_patched}/share/starship/presets/plain-text-symbols.toml";
      basePresetModified =
        ''
          add_newline=false
        ''
        + (mkDollarPrompt basePreset);
    in
    toString (pkgs.writeText "starship-config.toml" basePresetModified);
  programs.bash.interactiveShellInit = ''
    if [[ $TERM != "dumb" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "29.1,eat") ]]; then
      export STARSHIP_CACHE=/run/user/$UID/starship-cache
      eval "$(${starship_patched}/bin/starship init bash --print-full-init)"
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
