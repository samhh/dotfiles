{ config, ... }:

let webPort = 5232;
in
{
  age.secrets.radicale-htpasswd = {
    file = ../../../secrets/radicale-htpasswd.age;
    owner = "radicale";
  };

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
