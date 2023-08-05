{ config, pkgs, tshmPlugin, ... }:

{
  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings.auto-optimise-store = true;
  };

  environment.systemPackages = with pkgs; [ git ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${config.username}.imports = [ ./programs ];
    extraSpecialArgs = { inherit tshmPlugin; };
  };
}
