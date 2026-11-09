{
  description = "Configuring the universe with Nix";

  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      home-manager,
      nixpkgs,
      ...
    }:
    let
      system = "aarch64-darwin";
      system-ci = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      pkgs-ci = import nixpkgs { system = system-ci; };
    in
    {
      homeConfigurations.sam = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home ];
      };

      devShells = {
        ${system}.default = pkgs.callPackage ./shell.nix { };
        ${system-ci}.ci = pkgs-ci.callPackage ./shell.nix { };
      };

      formatter = {
        ${system} = pkgs.nixfmt-tree;
        ${system-ci} = pkgs-ci.nixfmt-tree;
      };
    };
}
