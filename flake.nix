{
  inputs =
    {
      nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

      home-manager = {
        url = "github:nix-community/home-manager/master";
        inputs.nixpkgs.follows = "nixpkgs";
      };
    };

  outputs = { nixpkgs, home-manager, ... }:
    let
      host = "alakazam";
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;

        config.allowUnfree = true;
      };

      lib = nixpkgs.lib;
    in {
      nixosConfigurations.${host} = lib.nixosSystem {
        inherit pkgs system;

        modules = [
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
          ./audio.nix
          ./backup.nix
          ./core.nix
          ./de.nix
          ./editor.nix
          ./email.nix
          ./gaming.nix
          ./hardware.nix
          ./misc.nix
          ./network.nix
          ./rss.nix
          ./security.nix
          ./terminal.nix
          ./user.nix
          ./vcs.nix
          ./web.nix
        ];
      };
    };
}
