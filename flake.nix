{
  description = "Configuring the universe with Nix";

  inputs = {
    catppuccin = {
      url = "github:catppuccin/nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    {
      self,
      catppuccin,
      home-manager,
      nixpkgs,
      nixpkgs-unstable,
    }:
    let
      system = "aarch64-darwin";
      system-ci = "x86_64-linux";
      overlays = with nixpkgs.lib; [
        (const (const self.packages.${system}))
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      pkgs-ci = import nixpkgs { system = system-ci; };
      pkgs-unstable = import nixpkgs-unstable { inherit system overlays; };
    in
    {
      homeConfigurations.sam = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          catppuccin.homeModules.catppuccin
          ./home
        ];
        extraSpecialArgs = {
          inherit pkgs-unstable;
        };
      };

      devShells = {
        ${system}.default = pkgs.callPackage ./shell.nix { };

        "${system-ci}".ci = pkgs-ci.mkShell { nativeBuildInputs = with pkgs-ci; [ deadnix ]; };
      };

      packages.${system} = import ./packages { inherit pkgs; };

      formatter = {
        ${system} = pkgs.nixfmt-rfc-style;
        ${system-ci} = pkgs-ci.nixfmt-rfc-style;
      };

      templates = import ./templates;
    };
}
