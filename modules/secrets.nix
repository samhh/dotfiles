{ agenix, system, ... }:

{
  age = {
    identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    secrets = {
      ddns-token.file = ../secrets/ddns-token.age;
      pihole-pass.file = ../secrets/pihole-pass.age;
      radicale-htpasswd = {
        file = ../secrets/radicale-htpasswd.age;
        owner = "radicale";
      };
    };
  };

  environment.systemPackages = [ agenix.defaultPackage.${system} ];
}
