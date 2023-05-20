{ pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      ublock-origin
      vimium
      darkreader
      # tridactyl
      # https-everywhere
    ];
    # TODO use firefox esr
    profiles = {
      default = {
        isDefault = true;
        settings = {
          "browser.display.background_color" = "#bdbdbd";
          "browser.search.hiddenOneOffs" =
            "Google,Yahoo,Bing,Amazon.com,Twitter";
          "browser.search.suggest.enabled" = false;
          "browser.startup.page" = 3;
          "browser.tabs.closeWindowWithLastTab" = false;
          "browser.urlbar.placeholderName" = "DuckDuckGo";
          "browser.urlbar.dnsResolveSingleWordsAfterSearch" = 0;
          "extensions.pocket.enabled" = false;
          "general.smoothScroll" = false;
          "layout.css.devPixelsPerPx" = "2";
          "network.IDN_show_punycode" = true;
          "network.allow-experiments" = false;
          "signon.rememberSignons" = false;
          # "widget.content.gtk-theme-override" = "Adwaita:light";
          "pdfjs.enableScripting" = false;
          "gfx.webrender.all" = true; # about:support Ctrl-f webrender
          "devtools.theme" = "dark";
          "ui.systemUsesDarkTheme" = 1;
          # Disable CORS for file:// . Needed for some webapps.
          "privacy.file_unique_origin" = false;
          "dom.image-lazy-loading.enabled" = false;
          # TODO find disable animation
        };
      };
    };
  };

}
