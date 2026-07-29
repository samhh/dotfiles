{ ... }:

{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    settings."*" = {
      IdentityFile = "~/.ssh/id_ed25519";
    };
  };
}
