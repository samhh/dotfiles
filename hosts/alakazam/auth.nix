{ uname, ... }:

{
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "qt";
  };

  programs.ssh = {
    startAgent = true;
    extraConfig = ''
      AddKeysToAgent yes
    '';
  };
}
