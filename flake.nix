{
  inputs =
    {
      agenix.url = "github:ryantm/agenix";

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

  outputs = { agenix, darwin, flake-utils, home-manager, nixpkgs, tshm-plugin, ... }:
    let
      selfpkgs = prev: {
        bangin = prev.callPackage ./pkgs/bangin.nix { };
        bangin-server-node = prev.callPackage ./pkgs/bangin-server-node.nix { };
        bangup = prev.callPackage ./pkgs/bangup { };
        corrupter = prev.callPackage ./pkgs/corrupter.nix { };
        proton-ge = prev.callPackage ./pkgs/proton-ge.nix { };
        qbpm = prev.callPackage ./pkgs/qbpm.nix { };
        tofi = prev.callPackage ./pkgs/tofi.nix { };
        tshm = prev.callPackage ./pkgs/tshm.nix { };
      };

      overlay-selfpkgs = final: prev: selfpkgs prev // {
        fishPlugins = prev.fishPlugins // {
          fish-minimal-theme = prev.callPackage ./pkgs/fishPlugins/fish-minimal-theme.nix { };
          z = prev.callPackage ./pkgs/fishPlugins/z.nix { };
        };

        vimPlugins = prev.vimPlugins // {
          exrc-vim = prev.callPackage ./pkgs/vimPlugins/exrc-vim.nix { };
          eyeliner-nvim = prev.callPackage ./pkgs/vimPlugins/eyeliner-nvim.nix { };
          nvim-surround = prev.callPackage ./pkgs/vimPlugins/nvim-surround.nix { };
          vim-just = prev.callPackage ./pkgs/vimPlugins/vim-just.nix { };
        };
      };

      # Usable both here and in module `config`s.
      localCfg = import ./shared/config.nix { };
      globalCfg = {
        imports = [ ./modules/config.nix ];
        config = localCfg;
      };

      homeManagerCfg = {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;

        home-manager.users.${localCfg.username}.imports = [
          ./modules/home-manager
        ];
      };

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
      }
    )) //

    (
      let
        system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.${system} = selfpkgs pkgs;
      }
    ) //

    {
      nixosConfigurations = {
        alakazam =
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
              inherit system;

              overlays = [ overlay-selfpkgs ];

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
          in
          nixpkgs.lib.nixosSystem {
            inherit pkgs system;

            modules = [
              home-manager.nixosModules.home-manager
              homeManagerCfg
              agenix.nixosModule
              globalCfg
              ./hosts/alakazam
            ];

            specialArgs = {
              inherit agenix system;

              tshmPlugin = tshm-plugin;

              apps = {
                terminal.bin = "${pkgs.foot}/bin/foot";
                launcher.bin = "${pkgs.tofi}/bin/tofi";
                webBrowser.bin = "${pkgs.qutebrowser}/bin/qutebrowser";
                streamer.bin = "${pkgs.streamlink}/bin/streamlink";
              };
            };
          };

        tentacool =
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
              inherit system;

              overlays = [ overlay-selfpkgs ];
            };
          in
          nixpkgs.lib.nixosSystem {
            inherit pkgs system;

            modules = [
              agenix.nixosModule
              globalCfg
              ./hosts/tentacool
            ];

            specialArgs = {
              inherit agenix system;
            };
          };
      };

      darwinConfigurations.lapras =
        let
          system = "aarch64-darwin";
          pkgs = import nixpkgs {
            inherit system;

            overlays = [ overlay-selfpkgs ];
          };
        in
        darwin.lib.darwinSystem {
          inherit pkgs system;

          modules = [
            home-manager.darwinModules.home-manager
            homeManagerCfg
            globalCfg
            ./hosts/lapras
          ];

          specialArgs = {
            tshmPlugin = tshm-plugin;
          };
        };
    };
}
