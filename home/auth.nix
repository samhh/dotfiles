{ ... }:

{
  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";
    extraConfig = ''
      IdentityFile = ~/.ssh/id_ed25519
    '';
  };
}
