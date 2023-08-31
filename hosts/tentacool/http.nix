{ config, ... }:

{
  security.acme = {
    acceptTerms = true;
    defaults.email = config.email.address;
  };

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    # When nginx can't match a request to a vhost it defaults to the first
    # defined block. We can access that by connecting directly to this host's
    # IP. This isn't really important, but it's a bit messy.
    #
    # Nix writes the vhosts to the config in alphabetical order, hence the name
    # of this vhost.
    virtualHosts."_".locations."/".return = "418";
  };
}
