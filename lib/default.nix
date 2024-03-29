{ pkgs, lib ? pkgs.lib }:

rec {

  mapNumToString = lib.elemAt (lib.splitString "" "abcdefghijklmnopqrstuvwxyz");

  mapStringToNum = str:
    let
      thelist = lib.imap0 (i: v: { inherit i v; })
        (lib.splitString "" "abcdefghijklmnopqrstuvwxyz");
    in
    (lib.findFirst (x: x.v == str)
      (throw "Element not found in list iteration")
      thelist).i;

  mkEmacsScreenshot =
    {
      # the code that should be executed before taking the screenshot
      emacsCode
      # change this if you want another format
    , name ? "emacs-screenshot.png"
    , emacs ? pkgs.emacs
    , light ? true
    , ...
    }:

    let
      screenshotScript = pkgs.writeText "script.el" ''
        (run-at-time 10 nil (lambda () (kill-emacs 1)))   ; fallback killing
        (load-theme 'modus-${if light then "operandi" else "vivendi"} t )
        (menu-bar-mode -1) ; 3
        (tool-bar-mode -1)
        (toggle-scroll-bar -1)
        (message nil)                            ; clear out echo area
        (defun screenshot-this-frame-and-exit ()
          (kill-emacs
            (call-process "import" nil nil nil
                          "-window" (frame-parameter (car (frame-list)) 'window-id) (getenv "out"))))
        (run-at-time 2 nil #'screenshot-this-frame-and-exit)
      '';
    in
    pkgs.runCommand name
      {
        nativeBuildInputs = [
          (emacs.pkgs.withPackages (e: [ e.magit-section e.modus-themes ]))
          pkgs.xvfb-run
          pkgs.iosevka
          pkgs.imagemagick
        ];
        emacsCodeFile = pkgs.writeText "emacscode.el" emacsCode;
      } ''
      HOME=$PWD \
        xvfb-run --server-args="-screen 0 1024x576x24" \
            emacs --quick --fullscreen \
            -l modus-themes \
            --font Iosevka\ 18 \
            ${pkgs.emacs.pkgs.modus-themes.src} \
            -l ${screenshotScript} \
            -l $emacsCodeFile
    '';

  mkGitRepository = src:
    pkgs.runCommandLocal "repository.git"
      rec {
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

        # cleaner git repos without the hooks
        GIT_TEMPLATE_DIR = pkgs.emptyDirectory.outPath;
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

  mkMbsyncFetcher =
    { email
    , hostextra ? ""
    , tls1dot ? 3
    , package ? pkgs.isync
    , configHead ? null
    }:
    let
      # emailuser = elemAt (splitString "@" email) 0;
      emailhost = lib.elemAt (lib.splitString "@" email) 1;
      name = emailhost;
      configHeadLet =
        if configHead == null then ''
          Host ${hostextra}${emailhost}
          User ${email}
          PassCmd "pass ${emailhost} | head -1"'' else
          lib.removeSuffix "\n" configHead;
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
    in
    pkgs.writeShellScriptBin "mbsync-fetcher-${name}" ''
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
    in
    pkgs.writeShellScriptBin "msmtp-${name}" ''
      exec ${pkgs.msmtp}/bin/msmtp --file=${configfile} "$@"
    '';

  mkGitMirror = url:
    pkgs.runCommandLocal "git-mirror"
      {
        nativeBuildInputs = with pkgs; [ git cacert ];
        inherit url;
        # to prevent junk
        GIT_TEMPLATE_DIR = pkgs.emptyDirectory.outPath;
      } ''
      mkdir $out
      cd $out
      git clone --mirror $url .
    '';

  mkGitCloneSingleBranch = { url, rev, outputHash }@args:
    pkgs.runCommandLocal "${baseNameOf url}-clone"
      ({
        nativeBuildInputs = with pkgs; [ git cacert ];
        # to prevent junk
        GIT_TEMPLATE_DIR = pkgs.emptyDirectory.outPath;
        outputHashMode = "recursive";
        outputHashAlgo = "sha256";
      } // args) ''
      git init --bare $out
      git -C $out fetch $url $rev
      git -C $out update-ref HEAD $(git -C $out rev-list -n 1 FETCH_HEAD)
    '';
}
