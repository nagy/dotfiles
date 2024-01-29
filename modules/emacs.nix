{ config, pkgs, lib, ... }:

let
  customEmacsPackages = pkgs.emacs29-gtk3.pkgs.overrideScope' (self: super: {
    sotlisp = super.sotlisp.overrideAttrs {
      src = pkgs.fetchFromGitHub {
        owner = "nagy";
        repo = "speed-of-thought-lisp";
        rev = "39930acbc4e8674fe36613d7d0961b49d21bcf50";
        hash = "sha256-tfEZLqYhq9HasR8T/QVgkyXrivXTLMzw2hZWxwwhr9g=";
      };
    };
    memoize = super.memoize.overrideAttrs {
      src = pkgs.fetchFromGitHub {
        owner = "nagy";
        repo = "emacs-memoize";
        rev = "33fcd1ec5a93f3768c43904fecc68399a84b8924";
        hash = "sha256-00C8WLR7CVCnp/VPgAP564XpMmXkaaddmi1tXdEevZI=";
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
in { environment.systemPackages = [ emacsAndPackages pkgs.mu ]; }
