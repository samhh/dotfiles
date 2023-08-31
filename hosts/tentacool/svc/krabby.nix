{ config, ... }:

let webPort = 5232;
in
{
  services.radicale = {
    enable = true;
    settings.auth = {
      type = "htpasswd";
      htpasswd_encryption = "bcrypt";
      htpasswd_filename = config.age.secrets.radicale-htpasswd.path;
    };
  };

  services.nginx.virtualHosts."krabby.samhh.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString webPort}";
    };
  };
}
