{ ... }:

{
  security.acme = {
    acceptTerms = true;
    defaults.email = "hello@samhh.com";
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
