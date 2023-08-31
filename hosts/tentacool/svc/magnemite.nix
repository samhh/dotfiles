{ ... }:

let
  name = "magnemite";
  lanPort = 9696;
in
{
  services.prowlarr = {
    enable = true;
    openFirewall = true;
  };

  services.nginx.virtualHosts."${name}.samhh.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString lanPort}";
      proxyWebsockets = true;
    };
  };
}
