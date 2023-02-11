nixpkgs-iosevka-comfy-040:
{ pkgs, ... }:

{
  fonts = {
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "Iosevka Comfy" ];
        #   sansSerif = [ "" ];
        #   serif = [ "" ];
      };
    };
    enableDefaultFonts = true;
    fonts = with import nixpkgs-iosevka-comfy-040 { inherit (pkgs) system; };
      [
        # because line-height is too high with newest version
        iosevka-comfy.comfy
      ] ++ (with pkgs; [
        etBook # EtBembo https://edwardtufte.github.io/et-book/
        noto-fonts
        # Only one icon used: 〜
        # From font "file-icons". May be a bit overkill
        emacs-all-the-icons-fonts
      ]);
  };
}
