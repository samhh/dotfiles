{ pkgs, uname, ... }:

{
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  services.nix-daemon.enable = true;

  home-manager.users.${uname}.home.stateVersion = "22.05";
}
