{ config, pkgs, ... }:

let
  customEmacsPackages = pkgs.emacsPackagesFor config.services.emacs.package;
  nagy = customEmacsPackages.callPackage ../emacs { };
  emacsAndPackages = customEmacsPackages.withPackages (epkgs:
    (with epkgs;
      with epkgs.melpaPackages; [
        nagy.nagy-modus-themes
        nagy.nagy-nlinum
        nagy.nagy-formats
        nagy.nagy-quirky-shell-command
        nagy.nagy-pcap-converter
        nagy.nagy-elpher
        nagy.nagy-qrcode
        nagy.nagy-use-package
        nagy.nagy-misc
        nagy.nagy-emacs
        wat-mode

        nameless
        (sotlisp.overrideAttrs (old: {
          src = pkgs.fetchFromGitHub {
            owner = "nagy";
            repo = "speed-of-thought-lisp";
            rev = "bf1c906cfbd111d6d0218aa7b5f8f3d635d89083";
            hash = "sha256-GsrIqnz+hPR1S0SkvduYp0rzmSHYkDDzFVWsLlgYEuM=";
          };
        }))
        smart-mode-line

        vterm
        pdf-tools
        org-pdftools
        elfeed
        ob-mermaid
        triples
        bufler
        focus
        osm
        # devdocs
        ts
        lispy
        lispyville
        evil
        evil-numbers
        anaphora
        general

        lin
        ov
        visual-fill-column

        # hy-mode
        yaml-mode
        csv-mode

        org
        # org-brain
        # all-the-icons
        ascii-art-to-unicode
        org-superstar
        org-appear
        org-ref
        # org-fancy-priorities
        nix-mode
        mermaid-mode
        # adaptive-wrap
        lorem-ipsum
        corfu
        cape
        jq-mode
        rustic
        format-all
        sideline

        # password and secrets
        pass
        password-store
        password-store-otp

        # devops
        terraform-mode
        gitlab-ci-mode
        groovy-mode
        dockerfile-mode
        eldoc-box
        git-modes
        jinx
        tokei

        # native packages with elisp files
        pkgs.gforth
      ]));
in {
  environment.systemPackages = [
    emacsAndPackages
    (pkgs.mu.override { inherit (customEmacsPackages) emacs; })
    pkgs.gforth
  ];
}
