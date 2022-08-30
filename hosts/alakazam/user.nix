{ config, ... }:

{
  home-manager.users.${config.username}.xdg.userDirs = {
    enable = true;
    documents = "$HOME/docs";
    download = "$HOME/downloads";
    pictures = "$HOME/pics";
  };
}
