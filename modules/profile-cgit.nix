{ pkgs, ... }:

{
  environment.etc.cgitrc.text = ''
    # snapshots=tar.gz zip
    enable-http-clone=1
    enable-index-links=1
    enable-index-owner=0
    max-repo-count=200
    # clone-url=http://$HTTP_HOST/git/$CGIT_REPO_URL
    clone-url=http://$HTTP_HOST/$CGIT_REPO_URL

    section-from-path=1
    scan-path=/var/git
    virtual-root=/cgit/
  '';

  services.httpd = {
    enable = true;
    extraModules = [ "cgi" "autoindex" ];
    # extraConfig = ''
    #     ServerTokens ProductOnly
    #   '';
    virtualHosts = {
      "localhost" = {
        documentRoot = pkgs.emptyDirectory.outPath;
        extraConfig = ''
          ScriptAlias /cgit "${pkgs.cgit}/cgit/cgit.cgi/"
          RedirectMatch ^/$ /cgit/

          # the "/" alias breaks the icons, we need to repair them
          Alias "/icons/" "${pkgs.apacheHttpd}/icons/"
          Alias "/cgit.css" "${pkgs.cgit}/cgit/cgit.css"
          Alias "/cgit.png" "${pkgs.cgit}/cgit/cgit.png"
          Alias "/favicon.ico" "${pkgs.cgit}/cgit/favicon.ico"

          Alias "/" "/var/git/"

          <Location "/cgit/">
            Header set Content-Security-Policy "default-src 'self'; style-src 'self' 'unsafe-inline' ;"
          </Location>

          <Directory "/var/git">
            AllowOverride None
            Options Indexes
            Require all granted
          </Directory>
          <Directory "${pkgs.cgit}/cgit/">
            AllowOverride None
            Options None
            Require all granted
          </Directory>
          <Directory "${pkgs.cgit}/cgit/">
            AllowOverride None
            Options ExecCGI FollowSymlinks
            Require all granted
          </Directory>
          <Directory "${pkgs.apacheHttpd}/icons/">
            AllowOverride None
            Options -Indexes
            Require all granted
          </Directory>
        '';
      };
    };
  };
}
