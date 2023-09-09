{
  description = "Configuring the universe with Nix";

  inputs =
    {
      agenix = {
        url = "github:ryantm/agenix";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      deploy-rs.url = "github:serokell/deploy-rs";

      home-manager = {
        url = "github:nix-community/home-manager/release-23.05";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      nix-alien.url = "github:thiagokokada/nix-alien";

      nix-colors.url = "github:misterio77/nix-colors";

      nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";

      tshm-plugin = {
        url = "https://registry.yarnpkg.com/typescript-tshm-plugin/-/typescript-tshm-plugin-0.1.0.tgz";
        flake = false;
      };
    };

  outputs = { self, agenix, deploy-rs, home-manager, nix-alien, nix-colors, nixpkgs, tshm-plugin }:
    with nixpkgs.lib; let
      system = "x86_64-linux";

      overlay = final: prev:
        self.packages.${system} //
        {
          agenix = agenix.packages.${final.system}.default;
          nix-alien = nix-alien.packages.${final.system}.nix-alien;

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

      deployPkgs = import nixpkgs {
        inherit system;
        overlays = [
          deploy-rs.overlay
          (_self: super: { deploy-rs = { inherit (pkgs) deploy-rs; lib = super.deploy-rs.lib; }; })
        ];
      };

      baseCfg = {
        nix = {
          # Modern nix CLI
          registry.nixpkgs.flake = nixpkgs;
          # Legacy nix-* CLI
          nixPath = [ "nixpkgs=${nixpkgs}" ];
        };
      };

      commonModules = [
        home-manager.nixosModules.home-manager
        agenix.nixosModules.default
        (import ./common)
        (import ./config)
        baseCfg
      ];

    in
    {
      nixosConfigurations = {
        alakazam = nixosSystem {
          inherit pkgs system;
          modules = commonModules ++ [ ./hosts/alakazam ];

          specialArgs = {
            inherit nix-colors;
            tshmPlugin = tshm-plugin;
          };
        };

        tentacool = nixosSystem {
          inherit pkgs system;
          modules = commonModules ++ [ ./hosts/tentacool ];
        };
      };

      deploy.nodes.tentacool = {
        hostname = "tentacool";
        profiles.system = {
          path =
            deployPkgs.deploy-rs.lib.activate.nixos self.nixosConfigurations.tentacool;
          sshUser = self.nixosConfigurations.tentacool.config.username;
          user = "root";
          # For password-based sudo, see:
          #   https://github.com/serokell/deploy-rs/issues/78#issuecomment-1367467086
          sshOpts = [ "-t" ];
          magicRollback = false;
          # Local builds would require messing with trusted users or signature
          # paths, see:
          #   https://github.com/serokell/deploy-rs/issues/25#issuecomment-740262646
          remoteBuild = true;
        };
      };

      devShells.${system}.default = import ./shell.nix { inherit pkgs; };
      packages.${system} = import ./packages { inherit pkgs; };
      templates = import ./templates;
    };
}
