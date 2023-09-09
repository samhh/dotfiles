{ config, ... }:

{
  home-manager.users.${config.username}.programs.ssh = {
    enable = true;
    matchBlocks.tentacool.extraOptions = {
      RemoteCommand = "cd ~/dotfiles/; sh -l";
      RequestTTY = "force";
    };
  };
}
