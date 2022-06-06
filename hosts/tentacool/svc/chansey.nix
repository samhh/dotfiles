{ ... }:

{
  services.ombi.enable = true;

  services.nginx.virtualHosts."chansey.samhh.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:5000";
    };
  };

  services.ddclient.domains = [ "chansey" ];
}
