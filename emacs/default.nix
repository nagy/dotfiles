{ lib, pkgs, fetchFromGitHub, trivialBuild, elpaBuild, modus-themes, paren-face
, nlinum, general, evil, elpher }:

let
  doom = fetchFromGitHub {
    owner = "doomemacs";
    repo = "doomemacs";
    rev = "d5ccac5d71c819035fa251f01d023b3f94b4fba4";
    hash = "sha256-7AzL08qo5WLeJo10lnF2b7g6FdWnExVYS8yipNyAMMM=";
  };
in {

  nagy-formats = trivialBuild rec {
    pname = "nagy-formats";
    version = "unstable";
    dontUnpack = true;
    packageRequires = [ ];

    buildPhase = ''
      runHook preBuild
      cp ${./${pname}.el} $pname.el
      # emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
      emacs -L . --batch -f batch-byte-compile *.el
      runHook postBuild
    '';
  };

  nagy-nlinum = trivialBuild rec {
    pname = "nagy-nlinum";
    version = "unstable";
    dontUnpack = true;
    packageRequires = [ general nlinum ];
    buildPhase = ''
      runHook preBuild
      cp ${./${pname}.el} $pname.el
      # emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
      emacs -L . --batch -f batch-byte-compile *.el
      runHook postBuild
    '';
  };

  nagy-modus-themes = trivialBuild rec {
    pname = "nagy-modus-themes";
    version = "unstable";
    dontUnpack = true;
    packageRequires = [ modus-themes paren-face ];

    buildPhase = ''
      runHook preBuild
      addToEmacsLoadPath ${doom}/lisp
      cp ${./${pname}.el} $pname.el
      # emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
      emacs -L . --batch -f batch-byte-compile *.el
      runHook postBuild
    '';
  };

  nagy-elpher = trivialBuild rec {
    pname = "nagy-elpher";
    version = "unstable";
    dontUnpack = true;
    packageRequires = [ general evil elpher ];

    buildPhase = ''
      runHook preBuild
      addToEmacsLoadPath ${doom}/lisp
      cp ${./${pname}.el} $pname.el
      # emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
      emacs -L . --batch -f batch-byte-compile *.el
      runHook postBuild
    '';
  };
}
