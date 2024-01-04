{ config, pkgs, lib, ... }:

let
  patchedEmacs = config.services.emacs.package.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      (pkgs.fetchpatch {
        # lower gc latency, already merged upstream
        url =
          "https://github.com/emacs-mirror/emacs/commit/396f46d904ab7509476b0d824ec2e4d9a231a2df.patch";
        hash = "sha256-U+ms+g712eRWVN/9Tm2HjVuCc5Z3g4BT0PDKzvqXALE=";
      })
    ];
  });
  customEmacsPackages = (pkgs.emacsPackagesFor patchedEmacs).overrideScope'
    (self: super: {
      sotlisp = super.sotlisp.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "speed-of-thought-lisp";
          rev = "39930acbc4e8674fe36613d7d0961b49d21bcf50";
          hash = "sha256-tfEZLqYhq9HasR8T/QVgkyXrivXTLMzw2hZWxwwhr9g=";
        };
      };
      tablist = super.tablist.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "tablist";
          rev = "1de3a025066bb3c5649bcb6e75201bffa96595db";
          hash = "sha256-yFjbO/BK72vJVINky6v10yLpFQWXYVUi1j9zFkIsBIU=";
        };
      };
      anaphora = super.anaphora.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "anaphora";
          rev = "d40b018f55361042825a8c07a17abcb86a7794b2";
          hash = "sha256-ukT4Ftm09Kp/B/UPcgOWlwI0nPiWR9q48PSSoz5GI0w=";
        };
      };
      memoize = super.memoize.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "emacs-memoize";
          rev = "961765962f3803503406e03b5c59eaaa23da22c3";
          hash = "sha256-dXEalMx1mT6F7WmWsx5eCdXFXEzbMnv+iLMmIElO1HI=";
        };
      };
    });
  emacs = customEmacsPackages.emacs;
  emacsAndPackages = customEmacsPackages.withPackages (epkgs:
    (lib.attrValues (import ../emacs { inherit pkgs lib emacs; }))
    ++ (with epkgs; [
      treesit-grammars.with-all-grammars
      pdf-tools
      org-pdftools
      bufler

      pass
      password-store
      password-store-otp

      info-colors
      exwm
      centered-cursor-mode
      ht
      fn
      yasnippet
      yasnippet-snippets

      # json-par               # depends on json-mode, causes problem with assigning js-json-mode
      # dwim-coder-mode        # only works in -ts-modes

      # super-save # https://github.com/bbatsov/super-save
      # literate-calc-mode
      # https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/trace.el
      # dumb-jump # https://github.com/jacktasia/dumb-jump
      mu4e
      aggressive-indent
      # (assert consult-gh.version == 2.0; consult-gh)
    ]));
in
{ environment.systemPackages = [ emacsAndPackages pkgs.mu ]; }
