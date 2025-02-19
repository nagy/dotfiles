{
  config,
  pkgs,
  lib,
  nur,
  ...
}:

let
  emacs = pkgs.emacs30-gtk3;
  emacsPackages = pkgs.emacsPackagesFor emacs;
  customEmacsPackages = emacsPackages.overrideScope (
    self: super: {
      sotlisp = super.sotlisp.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "speed-of-thought-lisp";
          rev = "55eb75635490ec89c0903ccc21fd5c37fdb2a8d6";
          hash = "sha256-SZH4foUlazaJwlJAYGJNw2iTTvyQ6nrs1RhxppStILI=";
        };
      };
      memoize = super.memoize.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "emacs-memoize";
          rev = "985e95846b3442d0a9e87eeff2d8259ccaf0598f";
          hash = "sha256-EYq/3EPHvQSzdZ79eXONsyTcapr2CAQ6c14kHr5ug90=";
        };
      };
      hy-mode = super.hy-mode.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "nagy";
          repo = "hy-mode";
          rev = "202b05423fe6b520c8c5d5cc1b87134bfd2c89b6";
          hash = "sha256-buDciWz8nbf0a8M2IPUZpbyQPHSugZCYDZqwSKIQqFY=";
        };
      };
      elisp-reader = nur.repos.nagy.emacsPackages.elisp-reader;
      obvious = nur.repos.nagy.emacsPackages.obvious;
      emacspy = nur.repos.nagy.emacsPackages.emacspy;
      lua = super.lua.override {
        lua = pkgs.lua5_4;
      };
    }
  );
  emacsAndPackages = customEmacsPackages.withPackages (
    epkgs:
    (
      [
        epkgs.treesit-grammars.with-all-grammars
        epkgs.age
        epkgs.gptel
        epkgs.emacspy
        epkgs.lua

        epkgs.llama
        epkgs.loopy
        epkgs.loopy-dash

        # epkgs.passage
      ]
      ++ (lib.attrValues (
        nur.repos.nagy.lib.emacsMakeDirectoryScope {
          path = ../emacs;
          inherit epkgs;
        }
      ))
    )
  );
in
{
  environment.systemPackages = lib.mkIf config.services.xserver.enable [
    emacsAndPackages
    pkgs.mu
  ];

  # to allow "malloc-trim" to trim memory of emacs.
  boot.kernel.sysctl."kernel.yama.ptrace_scope" = lib.mkIf config.services.xserver.enable (
    lib.mkForce 0
  );
}
