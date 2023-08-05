{
  inputs =
    {
      agenix = {
        url = "github:ryantm/agenix";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      flake-utils.url = "github:numtide/flake-utils";

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

  outputs = { self, agenix, flake-utils, home-manager, nix-colors, nixpkgs, tshm-plugin }:
    with nixpkgs.lib; let
      overlay = system: final: prev:
        self.packages.${system} //
        {
          agenix = agenix.packages.${final.system}.default;

          fishPlugins = prev.fishPlugins // (final.callPackage ./pkgs/fishPlugins { });
          vimPlugins = prev.vimPlugins // (final.callPackage ./pkgs/vimPlugins { });
        };

      getPkgs = system: import nixpkgs {
        inherit system;

        overlays = [ (overlay system) ];

        config.allowUnfreePredicate = pkg:
          let pkgName = getName pkg;
          in
          builtins.elem pkgName [
            "1password"
            "obsidian"
            "slack"
          ] ||
          # Steam includes a few unfree packages.
          (builtins.match "^steam(-.*)?" pkgName != null);
      };

      baseModules = [
        {
          nix = {
            # Modern nix CLI
            registry.nixpkgs.flake = nixpkgs;
            # Legacy nix-* CLI
            nixPath = [ "nixpkgs=${nixpkgs}" ];
          };
        }
        agenix.nixosModules.default
        (import ./cfg)
      ];

      getSystem = { hostname, system, config }:
        let pkgs = getPkgs system;
        in
        {
          nixosConfigurations.${hostname} = nixosSystem {
            inherit pkgs system;

            modules = baseModules ++
              [
                home-manager.nixosModules.home-manager
                (import ./nixos)
                config
              ];

            specialArgs = {
              inherit nix-colors;
              tshmPlugin = tshm-plugin;
            };
          };
        };

    in
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;

          overlays = [ (const (const { agenix = agenix.packages.${system}.default; })) ];
        };
      in
      {
        devShells.default = import ./shell.nix { inherit pkgs; };

        packages =
          let isSupportedPlatform = pkg: ! pkg.meta.unsupported;
          in filterAttrs (const isSupportedPlatform) (import ./pkgs { inherit pkgs; });
      }
    )) //

    (
      let f = fold (sys: pipe sys [ getSystem recursiveUpdate ]) { };
      in f (import ./hosts { systems = flake-utils.lib.system; })
    ) //

    {
      templates = import ./templates;
    };
}
