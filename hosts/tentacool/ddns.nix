# Note that an A record for any given subdomain already needs to exist in order
# for DDNS to work. It'll also need manually cleaning up after the service is
# removed.

{ config, ... }:

{
  services.ddclient = {
    enable = true;
    protocol = "namecheap";
    username = "samhh.com";
    passwordFile = config.age.secrets.ddns-token.path;
    # The default provider is currently broken for SSL:
    #   https://github.com/ddclient/ddclient/issues/309
    #
    # A list of alternative providers is available here:
    #   https://github.com/ddclient/ddclient/blob/215d4679e4640c5975dd3983d3e2c0ebe7b0e140/ddclient.in#L107-L121
    use = "web, web=googledomains";
  };
}
