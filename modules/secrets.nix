{ agenix, system, ... }:

{
  age.identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

  environment.systemPackages = [ agenix.defaultPackage.${system} ];
}
