{ config, ... }:

{
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
  };

  users.users.${config.username}.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICF3PGCLbd7QTcz4cSYONosH7tyJFsncXDTA/qRBo7/A"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGuKXom00vzANpoBmdGnEgX5dD7QijVD9AOHdxn+G9cl"
  ];
}
