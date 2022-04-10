{ config, pkgs, ... }:

let
  myEmacs = config.services.emacs.package.pkgs.withPackages (epkgs:
    (with epkgs;
      with epkgs.melpaPackages; [
        vterm
        org
        pdf-tools
        org-pdftools

        pass

        evil

        org-brain
        ascii-art-to-unicode # for org-brain fancyness

        diminish

        org-superstar
        org-ql
        elfeed
        dired-narrow
        dired-subtree
        outshine
        focus

        flycheck

        disk-usage
        nix-update

        ts # timestamp library

        imenu-list # somehow the doom provided one does not work

        bufler

        magit-section
        magit
        forge
        transient
        modus-themes

        exwm

        sly
        sly-macrostep
        sly-repl-ansi-color

        telephone-line

        # ledger-mode
        company-ledger

        info-colors
        diminish

        lispy
        lispyville

        vertico
        embark
        marginalia
        orderless
        consult
        embark-consult

        centered-cursor-mode

        ov
        treemacs
        treemacs-all-the-icons
        treemacs-evil
        treemacs-magit
        treemacs-projectile
        sly
        s # string library

        jenkins
        gitlab-ci-mode
        k8s-mode
        kubernetes
        kubernetes-evil

        dogears
        org-bookmark-heading
        dired-collapse
        reformatter
        python-black
        lsp-pyright

        osm
        helpful
      ]));
in { environment.systemPackages = [ myEmacs ]; }
