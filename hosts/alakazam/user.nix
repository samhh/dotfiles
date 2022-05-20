{ uname, ... }:

{
  home-manager.users.${uname}.xdg.userDirs = {
    enable = true;
    documents = "$HOME/docs";
    download = "$HOME/downloads";
    pictures = "$HOME/pics";
  };
}
