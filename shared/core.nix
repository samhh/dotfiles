{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${config.username} = {
      home.stateVersion = "22.05";
      imports = [ ../modules/programs ];
    };
  };
}
