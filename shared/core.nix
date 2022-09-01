{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${config.username} = {
      home.stateVersion = "22.05";
      imports = [ ../modules ];
    };
  };
}
