{ pkgs, ... }:

let
  # because line-height is too high with newest version
  overrideBuildPlan = old: {
    buildPlan = pkgs.writeText "private-build-plans.toml"
      ((builtins.readFile old.buildPlan) + "\n" + ''
        [buildPlans.iosevka-comfy.metric-override]
        leading = 1100
      '');
  };
in {
  fonts = {
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "Iosevka Comfy" ];
        #   sansSerif = [ "" ];
        #   serif = [ "" ];
      };

      # these revert the config from `fonts.optimizeForVeryHighDPI`.
      antialias = true;
      hinting.enable = true;
      subpixel.lcdfilter = "default";
      subpixel.rgba = "rgb";
      includeUserConf = false;
    };
    enableDefaultFonts = true;
    fonts = with pkgs; [
      (iosevka-comfy.comfy.overrideAttrs overrideBuildPlan)
      etBook # EtBembo https://edwardtufte.github.io/et-book/
      noto-fonts
      # Only one icon used: 〜
      # From font "file-icons". May be a bit overkill
      emacs-all-the-icons-fonts
    ];
  };
}
