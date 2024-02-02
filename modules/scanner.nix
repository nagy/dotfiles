{
  hardware.sane.enable = true;
  users.users.user.extraGroups = [ "scanner" ];

  nixpkgs.overlays = [
    (final: prev: {
      # Fixes a problem that attempt to access /nix/store/.../var/lock .
      # Without this, the scanner is not detected.
      sane-backends = prev.sane-backends.overrideAttrs
        ({ configureFlags ? [ ], ... }: {
          configureFlags = configureFlags ++ [ "--disable-locking" ];
        });
    })
  ];
}
