{ config, ... }:

{
  programs.firefox = {
    enable = config.services.xserver.enable;
    policies = {
      Cookies = {
        AcceptThirdParty = "never";
        Behavior = "reject-tracker-and-partition-foreign";
        ExpireAtSessionEnd = true;
        Locked = false;
      };
      DisableAppUpdate = true;
      DisableFirefoxAccounts = true;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DisableProfileRefresh = true;
      DisableFeedbackCommands = true;
      DisableSetDesktopBackground = true;
      DontCheckDefaultBrowser = true;
      NoDefaultBookmarks = true;
      ManagedBookmarks = [
        { toplevel_name = "Bookmarks"; }
        {
          url = "https://news.ycombinator.com";
          name = "Hacker News";
        }
        {
          url = "https://github.com/NixOS/nixpkgs/pulse/daily";
          name = "Nixpkgs pulse daily";
        }
      ];
      Homepage = {
        URL = "about:home";
        Locked = false;
        StartPage = "homepage";
      };
      FirefoxHome = {
        Search = true;
        TopSites = false;
        SponsoredTopSites = false;
        Highlights = false;
        Pocket = false;
        SponsoredPocket = false;
        Snippets = false;
        Locked = false;
      };
      SearchSuggestEnabled = false;
      Extensions = {
        Install = [
          "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi"
          "https://addons.mozilla.org/firefox/downloads/latest/vimium-ff/latest.xpi"
          # "https://addons.mozilla.org/firefox/downloads/latest/noscript/latest.xpi"

          # absolute file paths work here as well
          # https://github.com/mozilla/policy-templates#policiesjson-47
        ];
        Uninstall = [
          "amazon@search.mozilla.org"
          "bing@search.mozilla.org"
          "google@search.mozilla.org"
          "wikipedia@search.mozilla.org"
        ];
      };
      "3rdparty" = {
        Extensions = {
          "uBlock0@raymondhill.net" = {
            adminSettings = {
              userSettings = {
                advancedUserEnabled = true;
                # Show more advanced popup
                popupPanelSections = 31;
              };
              # this gets overwritten, so do not use
              # dynamicFilteringString = ''
              #   behind-the-scene * * noop
              #   behind-the-scene * inline-script noop
              #   behind-the-scene * 1p-script noop
              #   behind-the-scene * 3p-script noop
              #   behind-the-scene * 3p-frame noop
              #   behind-the-scene * image noop
              #   behind-the-scene * 3p noop
              #   * * 3p block
              # '';
            };
          };
        };
      };
      SearchEngines = {
        Remove = [
          "Amazon"
          "Bing"
          # "Google"
          "Twitter"
          "Wikipedia"
          "Yahoo"
        ];
        # Default = "Random SearX";
        Default = "Google";
      };
    };
    preferences = {
      # "general.smoothScroll" = false;
      "browser.aboutConfig.showWarning" = false;
      "dom.image-lazy-loading.enabled" = false;
      "pdfjs.enableScripting" = false;
      # "browser.urlbar.dnsResolveSingleWordsAfterSearch" = 0;
      # "browser.display.background_color" = "#000000";

      "browser.safebrowsing.downloads.enabled" = false;
      "browser.safebrowsing.downloads.remote.enabled" = false;
      "browser.safebrowsing.downloads.remote.url" = "";
      "browser.safebrowsing.enabled" = false;
      "browser.safebrowsing.malware.enabled" = false;

      "browser.safebrowsing.phishing.enabled" = false;

      "network.dns.disablePrefetch" = true;
      "network.dns.disablePrefetchFromHTTPS" = true;
      "network.captive-portal-service.enabled" = false;
      # "geo.enabled" = false;
      # "beacon.enabled" = false;

      "browser.tabs.crashReporting.sendReport" = false;
      "browser.tabs.firefox-view" = false;

      "widget.gtk.overlay-scrollbars.enabled" = false;
      "widget.non-native-theme.scrollbar.size.override" = 20;
      "widget.non-native-theme.scrollbar.style" = 4; # sharp corners

      # "media.autoplay.default" = 0;
      # "media.autoplay.enabled" = true;
      # "media.eme.enabled" = false;
      "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
    };
  };

  # From https://nixos.wiki/wiki/Firefox
  # You can make Firefox use xinput2 by setting the MOZ_USE_XINPUT2 environment
  # variable. This improves touchscreen support and enables additional touchpad
  # gestures. It also enables smooth scrolling as opposed to the stepped
  # scrolling that Firefox has by default.
  environment.sessionVariables = {
    MOZ_USE_XINPUT2 = "1";
  };
}
