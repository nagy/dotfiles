pkgs:

with pkgs.lib; rec {

  mapNumToString = num:
    elemAt (splitString "" "abcdefghijklmnopqrstuvwxyz") num;

  mapStringToNum = str:
    let
      thelist = imap0 (i: v: { inherit i v; })
        (splitString "" "abcdefghijklmnopqrstuvwxyz");
    in (findFirst (x: x.v == str) (throw "Element not found in list iteration")
      thelist).i;

  mkBashCompletion = cmd: list:
    let
      first = head list;
      underscored = concatStringsSep "_" list;
      # TODO This should be wrapped in quotes
      spaced = concatStringsSep " " list;
      len_minus_1 = (length list) - 1;
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
    let
      # FIXME This should be surrounded by quotes
      spaced = concatStringsSep " " list;
    in (pkgs.writeShellScriptBin cmd ''exec ${spaced} "$@"'');

  mkShortCommand = cmd: list:
    pkgs.symlinkJoin {
      name = "shortcommand-${cmd}";
      paths = [ (mkBashCompletion cmd list) (mkShortCommandScript cmd list) ];
    };

  mkEmacsScreenshot = {
    # the code that should be executed before taking the screenshot
    emacsCode
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

  mkGitRepository = src:
    pkgs.runCommandLocal "repository.git" rec {
      inherit src;
      nativeBuildInputs = [ pkgs.git ];
      GIT_AUTHOR_NAME = src.meta.author or "root";
      GIT_AUTHOR_EMAIL = src.meta.email or "root@localhost";
      GIT_COMMITTER_NAME = src.meta.author or "root";
      GIT_COMMITTER_EMAIL = src.meta.email or "root@localhost";

      GIT_AUTHOR_DATE =
        "Sat, 03 Mar 1973 09:46:40 +0000"; # date -d@100000000 -R
      GIT_COMMITTER_DATE =
        "Sat, 03 Mar 1973 09:46:40 +0000"; # date -d@100000000 -R

    } ''
      mkdir build
      pushd build
      git -c init.defaultBranch=master init .
      if [[ -f $src ]] ; then
        filenamelocal=$(basename $src | sed 's/[a-z0-9A-Z]\+-\(.*\)/\1/g' )
        cp -v -- $src $filenamelocal
      else
        cp -rv -- $src/* .
      fi
      git add .
      git commit --allow-empty-message --message ""
      mv .git $out
      popd
    '';

  mkMbsyncFetcher = { email, hostextra ? "", tls1dot ? 3, package ? pkgs.isync
    , configHead ? null }:
    let
      # emailuser = elemAt (splitString "@" email) 0;
      emailhost = elemAt (splitString "@" email) 1;
      name = emailhost;
      configHeadLet = (if configHead == null then ''
        Host ${hostextra}${emailhost}
        User ${email}
        PassCmd "pass ${emailhost} | head -1"'' else
        removeSuffix "\n" configHead);
      configfile = pkgs.writeText "mbsync-config-${name}" ''
        IMAPAccount default
        ${configHeadLet}
        SSLType IMAPS
        SSLVersions TLSv1.${toString tls1dot}

        IMAPStore default-remote
        Account default

        MaildirStore default-local
        Subfolders Verbatim
        # The trailing "/" is important
        Path ./
        Inbox ./INBOX

        Channel default
        Far :default-remote:
        Near :default-local:
        Patterns *
        Create Both
        Expunge Both
        SyncState *
      '';
    in pkgs.writeShellScriptBin "mbsync-fetcher-${name}" ''
      # a small sanity check
      [[ ! -d "INBOX" ]] && exit 1
      exec ${package}/bin/mbsync --config=${configfile} --all "$@"
    '';

  mkMsmtpAccount = name: configtxt:
    let
      configfile = pkgs.writeTextFile {
        name = "msmtp-config-file-" + name;
        text = ''
          # -*- mode:conf-space; -*-
          account default
          auth on
          tls on
          tls_starttls off
          ${configtxt}
          syslog
        '';
      };
    in (pkgs.writeShellScriptBin "msmtp-${name}" ''
      exec ${pkgs.msmtp}/bin/msmtp --file=${configfile} "$@"
    '');

  mkGitMirror = url:
    pkgs.runCommandLocal "git-mirror" {
      nativeBuildInputs = with pkgs; [ git cacert ];
      inherit url;
      # to prevent junk
      GIT_TEMPLATE_DIR = pkgs.emptyDirectory.outPath;
    } ''
      mkdir $out
      cd $out
      git clone --mirror $url .
    '';
}
