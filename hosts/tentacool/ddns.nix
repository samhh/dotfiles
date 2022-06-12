# Note that an A record for any given subdomain already needs to exist in order
# for DDNS to work.

{ config, ... }:

{
  services.ddclient = {
    enable = true;
    protocol = "namecheap";
    username = "samhh.com";
    passwordFile = config.age.secrets.ddns-token.path;
  };
}
