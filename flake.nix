{
  description = "Configuring the universe with Nix";

  inputs = {
    catppuccin = {
      url = "github:catppuccin/nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      self,
      catppuccin,
      home-manager,
      nixpkgs,
    }:
    let
      system = "aarch64-darwin";
      system-ci = "x86_64-linux";
      overlays = [
        (_final: _prev: self.packages.${system})
        (
          _final: prev:
          import ./packages/builders.nix {
            inherit (prev) lib writeTextFile fish;
          }
        )
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      pkgs-ci = import nixpkgs { system = system-ci; };
    in
    {
      homeConfigurations.sam = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          catppuccin.homeModules.catppuccin
          ./home
        ];
      };

      devShells = {
        ${system}.default = pkgs.callPackage ./shell.nix { };
        ${system-ci}.ci = pkgs-ci.callPackage ./shell.nix { };
      };

      packages.${system} = import ./packages { inherit pkgs; };

      formatter = {
        ${system} = pkgs.nixfmt-tree;
        ${system-ci} = pkgs-ci.nixfmt-tree;
      };

      templates = import ./templates;
    };
}
