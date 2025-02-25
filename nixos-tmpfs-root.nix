{
  nixosEval ? import <nixpkgs/nixos/lib/eval-config.nix>,
  extraModules ? [ ],
}:

nixosEval {
  specialArgs = {
    # dot = import ./. { };
    nur = import <nur> {
      nurpkgs = import <nixpkgs> { };
      pkgs = import <nixpkgs> { };
    };
  };
  modules = [
    (
      {
        lib,
        modulesPath,
        nur,
        pkgs,
        ...
      }:
      {
        imports = [
          "${modulesPath}/installer/netboot/netboot-base.nix"
          ./modules/all.nix
        ];

        nixpkgs.config.packageOverrides = pkgs: { inherit nur; };
        networking.hostName = "l"; # for live
        netboot.squashfsCompression = "zstd -Xcompression-level 1";

        fileSystems."/mnt" = {
          device = "/dev/disk/by-label/NIXOS";
          fsType = "ext4";
          options = [ "noatime" ];
          neededForBoot = true;
        };
        fileSystems."/nix/.ro-store" = lib.mkForce {
          device = "/mnt/squashfs.img";
          depends = [ "/mnt" ];
          fsType = "squashfs";
          neededForBoot = true;
        };
        fileSystems."/nix/.rw-store" = lib.mkForce {
          device = "/mnt/nix-store";
          depends = [ "/mnt" ];
          fsType = "none";
          options = [
            "bind"
            "defaults"
          ];
          neededForBoot = true;
        };
        fileSystems."/nix/var" = lib.mkForce {
          device = "/mnt/nix-var";
          depends = [ "/mnt" ];
          fsType = "none";
          options = [
            "bind"
            "defaults"
          ];
          neededForBoot = true;
        };

        system.stateVersion = "24.11";
      }
    )
  ] ++ extraModules;
}
