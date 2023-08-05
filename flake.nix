{
  inputs =
    {
      agenix = {
        url = "github:ryantm/agenix";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      home-manager = {
        url = "github:nix-community/home-manager/release-23.05";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      nix-colors.url = "github:misterio77/nix-colors";

      nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";

      tshm-plugin = {
        url = "https://registry.yarnpkg.com/typescript-tshm-plugin/-/typescript-tshm-plugin-0.1.0.tgz";
        flake = false;
      };
    };

  outputs = { self, agenix, home-manager, nix-colors, nixpkgs, tshm-plugin }:
    with nixpkgs.lib; let
      system = "x86_64-linux";

      overlay = final: prev:
        self.packages.${system} //
        {
          agenix = agenix.packages.${final.system}.default;

          fishPlugins = prev.fishPlugins // (final.callPackage ./packages/fishPlugins { });
          vimPlugins = prev.vimPlugins // (final.callPackage ./packages/vimPlugins { });
        };

      isAllowedUnfree = pkg:
        let pkgName = getName pkg;
        in
        builtins.elem pkgName [
          "1password"
          "obsidian"
          "slack"
        ] ||
        # Steam includes a few unfree packages.
        (builtins.match "^steam(-.*)?" pkgName != null);

      pkgs = import nixpkgs {
        inherit system;
        overlays = [ overlay ];
        config.allowUnfreePredicate = isAllowedUnfree;
      };

      baseCfg = {
        nix = {
          # Modern nix CLI
          registry.nixpkgs.flake = nixpkgs;
          # Legacy nix-* CLI
          nixPath = [ "nixpkgs=${nixpkgs}" ];
        };
      };

      getSystem = hostCfg:
        nixosSystem {
          inherit pkgs system;

          modules =
            [
              home-manager.nixosModules.home-manager
              agenix.nixosModules.default
              (import ./common)
              (import ./config)
              baseCfg
              hostCfg
            ];

          specialArgs = {
            inherit nix-colors;
            tshmPlugin = tshm-plugin;
          };
        };

    in
    {
      nixosConfigurations = {
        alakazam = getSystem ./hosts/alakazam;
        tentacool = getSystem ./hosts/tentacool;
      };

      devShells.${system}.default = import ./shell.nix { inherit pkgs; };
      packages.${system} = import ./packages { inherit pkgs; };
      templates = import ./templates;
    };
}
