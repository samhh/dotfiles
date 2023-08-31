{ ... }:

let webPort = 5000;
in
{
  services.ombi.enable = true;

  services.nginx.virtualHosts."chansey.samhh.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString webPort}";
    };
  };
}
