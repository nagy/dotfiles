{ lib, pkgs, fetchFromGitHub, trivialBuild, elpaBuild, modus-themes, paren-face
, nlinum, general, evil, elpher, yaml-mode, dash, nameless }:

let
  doom = fetchFromGitHub {
    owner = "doomemacs";
    repo = "doomemacs";
    rev = "d5ccac5d71c819035fa251f01d023b3f94b4fba4";
    hash = "sha256-7AzL08qo5WLeJo10lnF2b7g6FdWnExVYS8yipNyAMMM=";
  };
  makeTrivialBuild = { pname, packageRequires ? [ ] }:
    trivialBuild {
      inherit pname packageRequires;
      version = "unstable";
      dontUnpack = true;

      buildPhase = ''
        runHook preBuild
        cp ${./.}/$pname.el $pname.el
        emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
        runHook postBuild
      '';
    };
in rec {

  nagy-formats = makeTrivialBuild {
    pname = "nagy-formats";
    packageRequires = [ yaml-mode wat-mode evil ];
  };

  nagy-quirky-shell-command =
    makeTrivialBuild { pname = "nagy-quirky-shell-command"; };

  nagy-pcap-converter = makeTrivialBuild { pname = "nagy-pcap-converter"; };

  nagy-nlinum = trivialBuild {
    pname = "nagy-nlinum";
    version = "unstable";
    dontUnpack = true;
    packageRequires = [ general nlinum ];
    buildPhase = ''
      runHook preBuild
      cp ${./.}/$pname.el $pname.el
      # emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
      emacs -L . --batch -f batch-byte-compile *.el
      runHook postBuild
    '';
  };

  nagy-modus-themes = trivialBuild {
    pname = "nagy-modus-themes";
    version = "unstable";
    dontUnpack = true;
    packageRequires = [ modus-themes paren-face ];

    buildPhase = ''
      runHook preBuild
      addToEmacsLoadPath ${doom}/lisp
      cp ${./.}/$pname.el $pname.el
      # emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
      emacs -L . --batch -f batch-byte-compile *.el
      runHook postBuild
    '';
  };

  nagy-elpher = makeTrivialBuild {
    pname = "nagy-elpher";
    packageRequires = [ general evil elpher ];
  };

  nagy-qrcode = makeTrivialBuild {
    pname = "nagy-qrcode";
    packageRequires = [ dash ];
  };

  nagy-use-package = makeTrivialBuild { pname = "nagy-use-package"; };

  nagy-misc = makeTrivialBuild {
    pname = "nagy-misc";
    packageRequires = [ nameless ];
  };
}
