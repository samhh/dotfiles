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
        agenix.nixosModule
        (import ./cfg)
        (import ./shared)
      ];

      sysModules = isNixOS:
        if isNixOS
        then [ home-manager.nixosModules.home-manager (import ./nixos) ]
        else [ home-manager.darwinModules.home-manager ];

      getSystem = { hostname, system, isNixOS, isHeadful }:
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
                ./hosts/${hostname}
              ];

            specialArgs =
              # This essentially acts as a `lib` overlay (an actual overlay
              # change only affects `pkgs.lib`).
              { lib = getLib pkgs; } //

              (
                if isHeadful
                then { tshmPlugin = tshm-plugin; }
                else { }
              );
          };
        };

      getSystems = fold (compose [ getSystem recursiveUpdate ]) { };

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

    (getSystems [
      {
        hostname = "alakazam";
        system = "x86_64-linux";
        isNixOS = true;
        isHeadful = true;
      }

      {
        hostname = "tentacool";
        system = "x86_64-linux";
        isNixOS = true;
        isHeadful = false;
      }

      {
        hostname = "lapras";
        system = "aarch64-darwin";
        isNixOS = false;
        isHeadful = true;
      }
    ]);
}
