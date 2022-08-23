{ pkgs, uname, ... }:

{
  home-manager.users.${uname} = {
    home.packages = with pkgs; [ gnupg ];

    home.file.".gnupg/gpg-agent.conf".text = ''
      pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
    '';

    home.file.".ssh/config".text = ''
      AddKeysToAgent yes
    '';
  };

  programs.gnupg.agent.enable = true;
}
