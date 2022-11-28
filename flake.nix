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

      nix-colors.url = "github:misterio77/nix-colors";

      nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

      tshm-plugin = {
        url = "https://registry.yarnpkg.com/typescript-tshm-plugin/-/typescript-tshm-plugin-0.1.0.tgz";
        flake = false;
      };
    };

  outputs = { self, agenix, darwin, flake-utils, home-manager, nix-colors, nixpkgs, tshm-plugin }:
    let
      getLib = { lib, ... }: lib // import ./lib { inherit lib; };
    in
    with getLib nixpkgs; let
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
          let pkgName = getName pkg;
          in
          builtins.elem pkgName [
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
        agenix.nixosModule
        (import ./cfg)
        (import ./shared)
      ];

      sysModules = isNixOS:
        if isNixOS
        then [ home-manager.nixosModules.home-manager (import ./nixos) ]
        else [ home-manager.darwinModules.home-manager ];

      getSystem = { hostname, system, isNixOS, isHeadful, config }:
        let
          pkgs = getPkgs system;
          cfgs =
            if isNixOS
            then "nixosConfigurations"
            else "darwinConfigurations";
          sys =
            if isNixOS
            then nixpkgs.lib.nixosSystem
            else darwin.lib.darwinSystem;
        in
        {
          ${cfgs}.${hostname} = sys {
            inherit pkgs system;

            modules = baseModules ++ sysModules isNixOS ++
              [
                (if isHeadful then import ./headful else { })
                config
              ];

            specialArgs =
              # This essentially acts as a `lib` overlay (an actual overlay
              # change only affects `pkgs.lib`).
              { lib = getLib pkgs; } //

              (if isHeadful then {
                inherit nix-colors;
                tshmPlugin = tshm-plugin;
              } else { });
          };
        };

    in
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;

          overlays = [ (const (const { agenix = agenix.defaultPackage.${system}; })) ];
        };
      in
      {
        devShells.default = import ./shell.nix { inherit pkgs; };

        packages = with pkgs.lib;
          let isSupportedPlatform = pkg: ! pkg.meta.unsupported;
          in filterAttrs (const isSupportedPlatform) (import ./pkgs { inherit pkgs; });
      }
    )) //

    (
      let f = fold (compose [ getSystem recursiveUpdate ]) { };
      in f (import ./hosts { systems = flake-utils.lib.system; })
    ) //

    {
      templates = import ./templates;
    };
}
