# You will receive a package that has a `ala` terminal launcher and multiple scripts called
# `ala-${name}` as defined by the hmmodules argument. With these scripts you can switch the setting
# of the running terminal to that of the corresponding home-manager module setting

{
  pkgs,
  lib ? pkgs.lib,
  alacritty ? pkgs.alacritty,
  # A attrset of home-manager modules that define alacritty configs. You can switch to these with the
  # `ala-${name}` script.
  hmmodules ? { },
}:

let
  alacrittyLiveConfigPath = "/run/user/$UID/alacritty-conf.json";
  getAlaText =
    hmmodule:
    let
      tomlFormat = pkgs.formats.toml { };
      cfg = (hmmodule { }).programs.alacritty;
      tomlFile = tomlFormat.generate "alacritty.toml" cfg.settings;
    in
    pkgs.writeText "ala-config" (lib.replaceStrings [ "\\\\" ] [ "\\" ] (builtins.readFile tomlFile));
  mkAlacrittySwitcher =
    name: configpath:
    (pkgs.writeShellScriptBin "ala-${name}" ''
      cat < '${getAlaText configpath}' > ${alacrittyLiveConfigPath}
    '');
in
pkgs.symlinkJoin {
  name = "ala-switchers";
  paths = [
    (pkgs.writeShellScriptBin "alacritty" ''
      exec ${alacritty}/bin/alacritty \
        --option live_config_reload=true \
        --config-file ${alacrittyLiveConfigPath} "$@"
    '')
  ] ++ (lib.mapAttrsToList mkAlacrittySwitcher hmmodules) ++ [ alacritty ];
}
