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
          rev = "3e862df12361848be978d366fd4d9b74ac37b6bf";
          hash = "sha256-cbuNNQjS6AMDIYsv5TRMysd+0aY02GZBY2Ada9EQ7ZY=";
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
      jq-mode = super.jq-mode.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "jq-mode";
          rev = "10f6edd81043374307e72cb279327b920716eb3b";
          hash = "sha256-oPhAJlsPdTnZfODqc2Yi7q9S7C6dsf/DAI/fXnj6Yao=";
        };
      };
    });
  emacs = customEmacsPackages.emacs;
  emacsAndPackages = customEmacsPackages.withPackages (epkgs:
    (lib.attrValues (import ../emacs { inherit pkgs lib emacs; }))
    ++ (with epkgs; [
      pdf-tools
      org-pdftools
      triples
      bufler

      visual-fill-column

      format-all

      # password and secrets
      pass
      password-store
      password-store-otp

      dired-narrow
      mu4e
    ]));
in
{ environment.systemPackages = [ emacsAndPackages pkgs.mu ]; }
