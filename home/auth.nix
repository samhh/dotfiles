{ ... }:

{
  programs.ssh = {
    enable = true;
    extraConfig = ''
      IdentityFile = ~/.ssh/id_ed25519
    '';
  };
}
