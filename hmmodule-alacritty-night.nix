{ pkgs, font, fontSize, ... }:

{
  # Post about making alacritty responsive:
  # https://arslan.io/2021/02/15/automatic-dark-mode-for-terminal-applications/
  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        size = fontSize;
        normal = { family = font; };
      };
      # disable url launcher
      hints.enabled = [ ];
      scrolling = { history = 100 * 1000; };
      # xterm colors
      # https://github.com/eendroroy/alacritty-theme/blob/master/themes/xterm.yaml
      colors = {
        primary = {
          background = "0x000000";
          foreground = "0xffffff";
        };
        # Normal colors
        normal = {
          black = "0x000000";
          red = "0xcd0000";
          green = "0x00cd00";
          yellow = "0xcdcd00";
          blue = "0x0000ee";
          magenta = "0xcd00cd";
          cyan = "0x00cdcd";
          white = "0xe5e5e5";
        };
        # Bright colors
        bright = {
          black = "0x7f7f7f";
          red = "0xff0000";
          green = "0x00ff00";
          yellow = "0xffff00";
          blue = "0x5c5cff";
          magenta = "0xff00ff";
          cyan = "0x00ffff";
          white = "0xffffff";
        };
      };
    };
  };
}
