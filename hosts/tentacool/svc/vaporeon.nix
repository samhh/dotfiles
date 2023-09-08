{ pkgs, ... }:

let
  dnsPort = 53;
  apiPort = 4000;
  router = "192.168.0.1";
in
{
  networking.firewall.allowedTCPPorts = [
    dnsPort
    apiPort
  ];
  networking.firewall.allowedUDPPorts = [
    dnsPort
  ];

  services.blocky = {
    enable = true;
    settings = {
      ports = {
        dns = dnsPort;
        http = apiPort;
      };

      # Blocky picks two servers at random and races them. This also has various
      # privacy implications.
      upstream.default = [
        "9.9.9.9" # Quad9
        "8.8.8.8" # Google
      ];

      # Point to the router for local hostname resolution...
      clientLookup.upstream = router;
      # ...as well as for non-FQDN hostnames e.g. `tentacool` (else blocky sees
      # `tentacool.localdomain`). This is also necessary for UniFi adoption.
      conditional.mapping."." = router;

      blocking = {
        blackLists = rec {
          personal = [
            "https://dbl.oisd.nl"
            "https://adaway.org/hosts.txt"
            "https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt"
            "https://s3.amazonaws.com/lists.disconnect.me/simple_malvertising.txt"
            "https://v.firebog.net/hosts/Easyprivacy.txt"
            "https://v.firebog.net/hosts/static/w3kbl.txt"
            "https://v.firebog.net/hosts/Prigent-Ads.txt"
            "https://v.firebog.net/hosts/Prigent-Crypto.txt"
            "https://zerodot1.gitlab.io/CoinBlockerLists/hosts_browser"
          ];

          # The way whitelists work in blocky is a bit surprising. Without
          # associated blocklists, whitelists actually work to block everything
          # not explicitly whitelisted. Like this they work as expected,
          # negating the associated blacklist.
          work = personal;
        };

        # At time of writing Nix outputs YAML via remarshal, which doesn't
        # preserve the inline format Blocky wants here in a round-trip, so
        # presumably it's not possible. Hence the use of a filepath.
        whiteLists =
          # Domains here will be matched exactly if not defined as regex, see:
          #   https://github.com/0xERR0R/blocky/issues/556#issuecomment-1150731243
          let
            personalWhitelist = pkgs.writeText "vaporeon-personal-whitelist" ''
              /(\.|^)sonos\.com$/
            '';
            workWhitelist = pkgs.writeText "vaporeon-work-whitelist" ''
              /(\.|^)unsplash\.com$/
              /(\.|^)replay\.io$/
              /(\.|^)speedcurve\.com$/
              analytics.google.com
              /(\.|^)adzerk\.net$/
            '';
          in
          {
            personal = [ personalWhitelist ];
            work = [ personalWhitelist workWhitelist ];
          };

        clientGroupsBlock = {
          default = [ "personal" ];
          "alakazam.localdomain" = [ "work" ];
        };

        # Similar to `caching.cacheTimeNegative`, it's more practical for
        # clients to be more reactive to configuration changes.
        blockTTL = "1m";

        # Briefly expose the network to unfiltered DNS to get the network up as
        # fast as possible when blocky restarts.
        startStrategy = "fast";
      };

      # If a query fails it might be that I'm fiddling with it so it's just a
      # temporary outage, see:
      #   https://github.com/0xERR0R/blocky/issues/287
      caching.cacheTimeNegative = "1m";
    };
  };
}
