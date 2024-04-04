{
  description = "Configuring the universe with Nix";

  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    snippets-ls = {
      url = "github:quantonganh/snippets-ls";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tshm-plugin = {
      url = "https://registry.yarnpkg.com/typescript-tshm-plugin/-/typescript-tshm-plugin-0.1.0.tgz";
      flake = false;
    };
  };

  outputs =
    {
      self,
      agenix,
      home-manager,
      nixpkgs,
      nixpkgs-unstable,
      snippets-ls,
      tshm-plugin,
    }:
    let
      system = "aarch64-darwin";
      system-ci = "x86_64-linux";
      overlays = with nixpkgs.lib; [
        (const (const self.packages.${system}))
        (const (const {
          agenix = agenix.packages.${system}.default;
          snippets-ls = snippets-ls.packages.${system}.snippets-ls;
        }))
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      pkgs-ci = import nixpkgs { system = system-ci; };
      pkgs-unstable = import nixpkgs-unstable { inherit system overlays; };
      pkgs-unstable-ci = import nixpkgs-unstable { system = system-ci; };
    in
    {
      nixosConfigurations.tentacool = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
        modules = [
          home-manager.nixosModules.home-manager
          agenix.nixosModules.default
          ./common
          ./config
          ./homelab
        ];
      };

      homeConfigurations.sam = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./common/home-manager/programs
          ./home
        ];
        extraSpecialArgs = {
          inherit pkgs-unstable tshm-plugin;
        };
      };

      devShells = {
        ${system}.default = pkgs.callPackage ./shell.nix { };

        "${system-ci}".ci = pkgs-ci.mkShell { nativeBuildInputs = with pkgs-ci; [ deadnix ]; };
      };

      packages.${system} = import ./packages { inherit pkgs; };

      formatter = {
        ${system} = pkgs-unstable.nixfmt-rfc-style;
        ${system-ci} = pkgs-unstable-ci.nixfmt-rfc-style;
      };

      templates = import ./templates;
    };
}
