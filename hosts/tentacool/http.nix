# Note that DDNS needs to be up and running before a certificate can be issued.

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
  };
}
