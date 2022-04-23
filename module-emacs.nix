{ config, ... }:

let
  systemEmacs = config.services.emacs.package;
  customEmacsPackages =
    systemEmacs.pkgs.overrideScope' (self: super: { emacs = systemEmacs; });
  emacsAndPackages = customEmacsPackages.withPackages (epkgs:
    (with epkgs;
      with epkgs.melpaPackages; [
        use-package
        vterm
        org
        pdf-tools
        org-pdftools
        org-bookmark-heading

        pass

        evil

        org-brain
        ascii-art-to-unicode # for org-brain fancyness

        org-superstar
        org-ql
        elfeed
        dired-narrow
        dired-subtree
        dired-collapse
        outshine
        focus

        flycheck

        disk-usage

        ts # timestamp library

        imenu-list # somehow the doom provided one does not work

        bufler

        magit-section
        magit
        forge
        transient
        modus-themes
        lin

        exwm

        # modeline
        telephone-line
        diminish

        # ledger-mode
        company-ledger

        info-colors

        # lisps
        lispy        # this pulls in ivy
        lispyville
        sly
        sly-macrostep
        sly-repl-ansi-color

        # completion
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
        s # string library

        dogears
        reformatter
        python-black
        lsp-pyright

        osm
        helpful
        yasnippet-snippets
      ]));
in { environment.systemPackages = [ emacsAndPackages ]; }
