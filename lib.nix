{ pkgs, lib ? pkgs.lib }:

rec {

  mapNumToString = num:
    builtins.elemAt (lib.splitString "" "abcdefghijklmnopqrstuvwxyz") num;

  mapStringToNum = str:
    let
      thelist = lib.imap0 (i: v: { inherit i v; })
        (lib.splitString "" "abcdefghijklmnopqrstuvwxyz");
    in (pkgs.lib.findFirst (x: x.v == str)
      (throw "Element not found in list iteration") thelist).i;

  mkBashCompletion = cmd: list:
    let
      first = lib.head list;
      underscored = lib.concatStringsSep "_" list;
      # TODO This should be wrapped in quotes
      spaced = lib.concatStringsSep " " list;
      len_minus_1 = (builtins.length list) - 1;
    in (pkgs.writeTextDir "share/bash-completion/completions/${cmd}" ''
      function _complete_shortcommand_${underscored} {
        local __COMPS
        ((COMP_CWORD+=${toString len_minus_1}))
        COMP_WORDS=( ${spaced} "${"$"}{COMP_WORDS[@]:1}" )
        # this is for exotic commands like git
        COMP_LINE="${spaced} ${"$"}{COMP_WORDS[@]:1}"
        # after COMP_LINE update we need to update this as well
        COMP_POINT=${"$"}{#COMP_LINE}
        __load_completion ${first}
        __COMPS="$(complete -p ${first}))"
        # check for completion function
        [[ $__COMPS =~ -F\ (_[-_0-9a-zA-Z]+) ]] && {
          # eval completion function
          ${"$"}{BASH_REMATCH[1]}
        }
        return 0
      }
      complete -F _complete_shortcommand_${underscored} ${cmd}
    '');

  mkShortCommandScript = cmd: list:
    let spaced = lib.concatStringsSep " " list;
    in (pkgs.writeShellScriptBin cmd ''exec ${spaced} "$@"'');

  mkShortCommand = cmd: list:
    pkgs.symlinkJoin {
      name = "shortcommand-${cmd}";
      paths = [ (mkBashCompletion cmd list) (mkShortCommandScript cmd list) ];
    };
}
