isDark:

{ ... }:

{
  # Post about making alacritty responsive:
  # https://arslan.io/2021/02/15/automatic-dark-mode-for-terminal-applications/
  programs.alacritty = {
    enable = true;
    settings = {
      # disable url launcher
      hints.enabled = [ ];
      # 100k is the max
      scrolling = {
        history = 100 * 1000;
      };
      # xterm colors
      # https://github.com/eendroroy/alacritty-theme/blob/master/themes/xterm.yaml
      colors = {
        primary = {
          background = if isDark then "0x000000" else "0xffffff";
          foreground = if isDark then "0xffffff" else "0x000000";
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
      keyboard.bindings = [
        {
          key = "PageUp";
          mode = "Vi|~Search";
          action = "ScrollPageUp";
        }
        {
          key = "PageDown";
          mode = "Vi|~Search";
          action = "ScrollPageDown";
        }
        {
          key = "Home";
          mode = "Vi|~Search";
          action = "ScrollToTop";
        }
        {
          key = "End";
          mode = "Vi|~Search";
          action = "ScrollToBottom";
        }
        {
          key = "Slash";
          mode = "Vi|~Search";
          action = "SearchForward";
          # even this this entry is already in the default
          # configuration, because this line is not present there, the
          # bindings has no effect on qwertz keyboards.
          mods = "Shift";
        }
      ];
    };
  };
}
