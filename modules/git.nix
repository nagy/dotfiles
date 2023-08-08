{ pkgs, ... }:

{

  programs.git = {
    enable = true;
    config = {
      alias = {
        # c = "commit"; # in included git aliases
        # co = "checkout"; # in included git aliases
        cl = "clone";
        cl1 = "clone --depth 1";
        f = "fetch";
        lol = "log --graph --decorate --pretty=oneline --abbrev-commit";
        lola = "lol --all";
      };
      user.name = "Daniel Nagy";
      user.email = "danielnagy@posteo.de";
      commit = {
        # Show my changes when writing the message
        verbose = true;
      };
      init = { defaultBranch = "master"; };
      push = { default = "current"; };
      pull.rebase = true;
      include.path =
        let
          git-alias = pkgs.fetchFromGitHub {
            owner = "GitAlias";
            repo = "gitalias";
            rev = "ed036c1fd16c8e690329c594bc028f58c6e3b349";
            sha256 = "sha256-OtKdN4SeJSswtF3Uvs3cMZwTwpL2wEm4KU1iKmfEr30=";
          };
        in
        "${git-alias}/gitalias.txt";
      merge.conflictStyle = "diff3";
      gc = { auto = "0"; };
      url = {
        # forges
        "https://github.com/".insteadOf = "gh:";
        "https://gist.github.com/".insteadOf = "gist:";
        "https://gitlab.com/".insteadOf = "gl:";
        "https://git.sr.ht/".insteadOf = "sh:";
        "https://codeberg.org/".insteadOf = "cb:";
        "https://aur.archlinux.org/".insteadOf = "aur:";
        "https://bitbucket.org/".insteadOf = "bb:";
        # nagy repos
        "git@github.com:nagy/".insteadOf = "ghn:";
        "git@gitlab.com:nagy/".insteadOf = "gln:";
        "git@git.sr.ht:~nagy/".insteadOf = "shn:";
        "git@codeberg.org:nagy/".insteadOf = "cbn:";
        # organizations
        "https://github.com/NixOS/".insteadOf = "nixos:";
        "https://github.com/rust-lang/".insteadOf = "rust:";
        "https://github.com/NixOS/nixpkgs".insteadOf = "pkgs:";
        "https://github.com/nix-community/NUR".insteadOf = "nur:";
      };
      tar = {
        "tar.xz".command = "${pkgs.xz}/bin/xz -c";
        "tar.bz2".command = "${pkgs.bzip2}/bin/bzip2 -c";
        "tar.zst".command = "${pkgs.zstd}/bin/zstd -c";
      };
      # Shiny colors
      color = {
        branch = "auto";
        diff = "auto";
        interactive = "auto";
        status = "auto";
        ui = "auto";
      };

      # Pretty much the usual diff colors
      "color.diff" = {
        commit = "yellow";
        frag = "cyan";
        meta = "yellow";
        new = "green";
        old = "red";
        whitespace = "red reverse";
      };

      "color.diff-highlight" = {
        oldNormal = "red bold";
        oldHighlight = "red bold 52";
        newNormal = "green bold";
        newHighlight = "green bold 22";
      };
      # To work around the workaround of CVE-2022-24765.
      # See https://github.com/NixOS/nixpkgs/issues/169193 for more
      # safe.directory = "*";
      filter = {
        # use with `.gitattributes`
        # file content: *.sqlite3 filter=sqlite3-sql
        # more info https://github.com/theTaikun/SQLite-git-smudge-and-clean
        sqlite3-sql = {
          clean = "${pkgs.sqlite}/bin/sqlite3 %f .dump";
          smudge = toString (pkgs.writeShellScript "git-smudge-sqlite3" ''
            TMPFILE=$(mktemp)
            cat | ${pkgs.sqlite}/bin/sqlite3 "$TMPFILE"
            cat -- "$TMPFILE"
            rm -f -- "$TMPFILE"
          '');
        };
        jq = { clean = "${pkgs.jq}/bin/jq --sort-keys"; };
      };
      diff = {
        wasm = {
          textconv = "${pkgs.wabt}/bin/wasm2wat";
          binary = true;
        };
        pdf = {
          textconv = pkgs.writeShellScript "pdftostdout" ''
            exec ${pkgs.poppler_utils}/bin/pdftotext -layout "$@" -
          '';
          binary = true;
        };
        tar = {
          textconv = "${pkgs.gnutar}/bin/tar -tvf";
          binary = true;
        };
        tar-gz = {
          textconv = "${pkgs.gnutar}/bin/tar -tvzf";
          binary = true;
        };
        tar-bz2 = {
          textconv = "${pkgs.gnutar}/bin/tar -tvjf";
          binary = true;
        };
        tar-xz = {
          textconv = "${pkgs.gnutar}/bin/tar -tvJf";
          binary = true;
        };
      };
    };
  };
  environment.etc.gitattributes.text = ''
    *.wasm diff=wasm
    *.pdf diff=pdf
    *.tar diff=tar
    *.tar.gz diff=tar-gz
    *.tgz diff=tar-gz
    *.tar.bz2 diff=tar-bz2
    *.tar.xz diff=tar-xz
    *.json filter=jq
  '';
}
