{ uname, ... }:

{
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "gtk2";
  };

  programs.ssh = {
    startAgent = true;
    extraConfig = ''
      AddKeysToAgent yes
    '';
  };
}
