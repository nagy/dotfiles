{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.firefox = {
    enable = config.services.xserver.enable;
    package = pkgs.firefox-esr;
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
      "general.smoothScroll" = false;
      "browser.aboutConfig.showWarning" = false;
      "dom.image-lazy-loading.enabled" = false;
      # "dom.iframe_lazy_loading.enabled" = false;
      "pdfjs.enableScripting" = false;
      # "browser.urlbar.dnsResolveSingleWordsAfterSearch" = 0;
      "browser.display.background_color.dark" = "#000000";

      # https://support.mozilla.org/en-US/questions/1287625
      "dom.push.enabled" = false;
      "dom.webnotifications.enabled" = false;
      "notification.feature.enabled" = false; # this was not present in about:config

      "browser.safebrowsing.downloads.enabled" = false;
      "browser.safebrowsing.downloads.remote.enabled" = false;
      "browser.safebrowsing.downloads.remote.url" = "";
      "browser.safebrowsing.enabled" = false;
      "browser.safebrowsing.malware.enabled" = false;
      "browser.safebrowsing.phishing.enabled" = false;

      "browser.translations.enable" = false;

      "network.dns.disablePrefetch" = true;
      "network.dns.disablePrefetchFromHTTPS" = true;
      "network.captive-portal-service.enabled" = false;
      # https://wiki.archlinux.org/title/Firefox/Privacy#Disable/enforce_'Trusted_Recursive_Resolver'
      "network.trr.mode" = 5;
      # "geo.enabled" = false;
      # "beacon.enabled" = false;
      # "network.stricttransportsecurity.preloadlist" = false;

      "browser.urlbar.suggest.calculator" = true;

      # no need to check for blocked extensions
      # "extensions.blocklist.enabled" = false;

      "browser.tabs.crashReporting.sendReport" = false;
      "browser.tabs.firefox-view" = false;
      # "browser.fixup.dns_first_for_single_words" = true;
      "browser.fixup.domainsuffixwhitelist.meship" = true;
      "browser.fixup.domainsuffixwhitelist.ygg" = true;
      "browser.fixup.domainsuffixwhitelist.anon" = true;
      # OpenNIC
      "browser.fixup.domainsuffixwhitelist.glue" = true;
      "browser.fixup.fallback-to-https" = false;

      "widget.gtk.overlay-scrollbars.enabled" = false;
      "widget.non-native-theme.scrollbar.size.override" = 20;
      "widget.non-native-theme.scrollbar.style" = 4; # sharp corners

      "media.autoplay.default" = 5; # block audio and video by default
      # "media.autoplay.enabled" = true;
      # "media.eme.enabled" = false;
      "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
      "media.webspeech.synth.enabled" = false;

      # https://support.mozilla.org/en-US/kb/privacy-preserving-attribution?as=u&utm_source=inproduct
      # https://michael.kjorling.se/blog/2024/disabling-privacy-preserving-ad-measurement-in-firefox-128/
      "dom.private-attribution.submission.enabled" = false;

      # no more search by url bar typing
      "keyword.enabled" = false;

      # disable favicons
      "browser.chrome.site_icons" = false;

      # HIDDEN PREF: disable recommendation pane in about:addons (uses Google Analytics)
      "extensions.getAddons.showPane" = false;
      # recommendations in about:addons' Extensions and Themes panes [FF68+]
      "extensions.htmlaboutaddons.recommendations.enabled" = false;

      # from here https://git.sr.ht/~toastal/nixcfg/tree/trunk/item/program/browser/firefox/settings.nix
      "browser.uidensity" = 1;

      # disable open264 plugin
      "media.gmp-gmpopenh264.enabled" = false;

      "media.navigator.enabled" = false; # Disable mic and camera
      "media.peerconnection.enabled" = false; # Disable WebRTC
    };
  };

  # Firefox connects to these hosts on every start.
  # Hard disable them on the whole host.
  networking.hosts = {
    "0.0.0.0" = [
      "content-signature-2.cdn.mozilla.net"
      # maybe this one can still be disabled via `about:config`
      "firefox.settings.services.mozilla.com"
    ];
  };

  # From https://nixos.wiki/wiki/Firefox
  # You can make Firefox use xinput2 by setting the MOZ_USE_XINPUT2 environment
  # variable. This improves touchscreen support and enables additional touchpad
  # gestures. It also enables smooth scrolling as opposed to the stepped
  # scrolling that Firefox has by default.
  environment.sessionVariables = lib.mkIf config.services.xserver.enable { MOZ_USE_XINPUT2 = "1"; };
}
