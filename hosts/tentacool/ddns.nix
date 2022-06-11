{ config, ... }:

{
  services.ddclient = {
    enable = true;
    protocol = "namecheap";
    username = "samhh.com";
    passwordFile = config.age.secrets.ddns-token.path;
  };
}
