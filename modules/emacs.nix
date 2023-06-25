{ config, pkgs, lib, ... }:

let
  customEmacsPackages = pkgs.emacsPackagesFor config.services.emacs.package;
  emacsAndPackages = customEmacsPackages.withPackages (epkgs:
    (lib.attrValues (import ../emacs {
      inherit pkgs lib;
      inherit (epkgs) emacs;
    })) ++ (with epkgs;
      with epkgs.melpaPackages; [
        wat-mode

        (sotlisp.overrideAttrs (_: {
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
        triples
        bufler
        focus
        osm
        # devdocs
        ts
        lispy
        lispyville

        ov
        visual-fill-column

        yaml-mode
        csv-mode

        org
        ascii-art-to-unicode
        org-superstar
        org-appear
        org-ref
        # org-fancy-priorities
        nix-mode
        mermaid-mode
        ob-mermaid
        # adaptive-wrap
        lorem-ipsum
        corfu
        cape
        jq-mode
        rustic
        format-all

        # password and secrets
        pass
        password-store
        password-store-otp

        # devops
        terraform-mode
        gitlab-ci-mode
        groovy-mode
        dockerfile-mode
        jenkinsfile-mode
        jenkins

        git-modes
        jinx
        tokei
        dired-narrow
      ]));
in {
  environment.systemPackages = [
    emacsAndPackages
    (pkgs.mu.override { inherit (customEmacsPackages) emacs; })
  ];
}
