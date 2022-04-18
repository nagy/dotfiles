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

  mkEmacsScreenshot = { pkgs
    # the code that should be executed before taking the screenshot
    , emacsCode
    # change this if you want another format
    , name ? "emacs-screenshot.png", lib ? pkgs.lib, emacs ? pkgs.emacs
    , imagemagick ? pkgs.imagemagick, light ? true, ... }:

    let
      Myemacs = emacs.pkgs.withPackages (e: [ e.magit-section e.modus-themes ]);
      emacsCodeFile = pkgs.writeText "emacscode.el" emacsCode;
      screenshotScript = pkgs.writeText "script.el" ''
        (run-at-time 10 nil (lambda () (kill-emacs 1)))   ; fallback killing
        (require 'modus-themes)
        (load-theme 'modus-${if light then "operandi" else "vivendi"} t )
        (menu-bar-mode -1) ; 3
        (tool-bar-mode -1)
        (toggle-scroll-bar -1)
        (message nil)                            ; clear out echo area
        (defun screenshot-this-frame-and-exit ()
          (let ((window-id (frame-parameter (car (frame-list)) 'window-id)))
            (kill-emacs
              (call-process "import" nil nil nil
                            "-window" window-id (getenv "out")))))
        (run-at-time 2 nil #'screenshot-this-frame-and-exit)
      '';
    in pkgs.runCommand name {
      nativeBuildInputs =
        [ Myemacs pkgs.xvfb-run pkgs.iosevka pkgs.imagemagick ];
    } ''
      HOME=$PWD \
        xvfb-run --server-args="-screen 0 1024x576x24" \
            emacs --quick --fullscreen \
            --font Iosevka\ 18 \
            ${pkgs.emacs.pkgs.melpaPackages.modus-themes.src} \
            -l ${screenshotScript} \
            -l ${emacsCodeFile}
    '';

  mkGitRepository = { pkgs, lib ? pkgs.lib, src, ... }:
    pkgs.runCommand "repository.git" rec {
      inherit src;
      nativeBuildInputs = [ pkgs.git ];
      GIT_AUTHOR_NAME = src.meta.author or "root";
      GIT_AUTHOR_EMAIL = src.meta.email or "root@localhost";
      GIT_COMMITTER_NAME = src.meta.author or "root";
      GIT_COMMITTER_EMAIL = src.meta.email or "root@localhost";

      GIT_AUTHOR_DATE =
        "Sat, 03 Mar 1973 10:46:40 +0100"; # date -d@100000000 -R
      GIT_COMMITTER_DATE =
        "Sat, 03 Mar 1973 10:46:40 +0100"; # date -d@100000000 -R

    } ''
      git -c init.defaultBranch=master init .
      filenamelocal=$(basename $src | sed 's/[a-z0-9A-Z]\+-\(.*\)/\1/g' )
      cp -v -- $src $filenamelocal
      git add $filenamelocal
      git commit --allow-empty-message --message ""
      mv .git $out
    '';
}
