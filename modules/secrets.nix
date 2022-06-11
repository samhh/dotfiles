{ agenix, system, ... }:

{
  age = {
    identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    secrets = {
      ddns-token.file = ../secrets/ddns-token.age;
    };
  };

  environment.systemPackages = [ agenix.defaultPackage.${system} ];
}
