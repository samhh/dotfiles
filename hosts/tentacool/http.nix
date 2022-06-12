# Note that DDNS needs to be up and running before a certificate can be issued.

{ email, ... }:

{
  security.acme = {
    acceptTerms = true;
    defaults = { inherit email; };
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