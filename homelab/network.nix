{ ... }:

{
  networking = {
    hostName = "homelab";
    interfaces.eno1.ipv6.addresses = [
      {
        address = "fd1d:8607:ebdd:b24b::1";
        prefixLength = 64;
      }
    ];
  };
}
