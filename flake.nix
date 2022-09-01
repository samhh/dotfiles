{
  inputs =
    {
      agenix = {
        url = "github:ryantm/agenix";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      darwin = {
        url = "github:lnl7/nix-darwin";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      flake-utils.url = "github:numtide/flake-utils";

      home-manager = {
        url = "github:nix-community/home-manager/master";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

      tshm-plugin = {
        url = "https://registry.yarnpkg.com/typescript-tshm-plugin/-/typescript-tshm-plugin-0.1.0.tgz";
        flake = false;
      };
    };

  outputs = { self, agenix, darwin, flake-utils, home-manager, nixpkgs, tshm-plugin }:
    let
      overlay = system: final: prev:
        self.packages.${system} //
        {
          agenix = agenix.defaultPackage.${final.system};

          fishPlugins = prev.fishPlugins // (final.callPackage ./pkgs/fishPlugins { });
          vimPlugins = prev.vimPlugins // (final.callPackage ./pkgs/vimPlugins { });
        };

      getPkgs = system: import nixpkgs {
        inherit system;

        overlays = [ (overlay system) ];

        config.allowUnfreePredicate = pkg:
          let pkgName = nixpkgs.lib.getName pkg;
          in
          builtins.elem pkgName [
            "obsidian"
            "slack"
          ] ||
          # Steam includes a few unfree packages.
          (builtins.match "^steam(-.*)?" pkgName != null);
      };

      baseModules = pkgs: [
        agenix.nixosModule

        (
          if pkgs.stdenv.isDarwin
          then home-manager.darwinModules.home-manager
          else home-manager.nixosModules.home-manager
        )

        (import ./config)
        (import ./shared/core.nix)
      ];

    in
    (flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            git
            git-crypt
            nixpkgs-fmt
          ];
        };

        packages = with pkgs.lib;
          let isSupportedPlatform = pkg: ! pkg.meta.unsupported;
          in filterAttrs (const isSupportedPlatform) (import ./pkgs { inherit pkgs; });
      }
    )) //

    {
      nixosConfigurations = {
        alakazam =
          nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            pkgs = getPkgs system;

            modules = baseModules pkgs ++ [ ./hosts/alakazam ];

            specialArgs = {
              tshmPlugin = tshm-plugin;
            };
          };

        tentacool =
          nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            pkgs = getPkgs system;

            modules = baseModules pkgs ++ [ ./hosts/tentacool ];
          };
      };

      darwinConfigurations.lapras =
        darwin.lib.darwinSystem rec {
          system = "aarch64-darwin";
          pkgs = getPkgs system;

          modules = baseModules pkgs ++ [ ./hosts/lapras ];

          specialArgs = {
            tshmPlugin = tshm-plugin;
          };
        };
    };
}
