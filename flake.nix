{
  inputs =
    {
      nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

      darwin.url = "github:lnl7/nix-darwin";
      darwin.inputs.nixpkgs.follows = "nixpkgs";

      flake-utils.url = "github:numtide/flake-utils";

      home-manager = {
        url = "github:nix-community/home-manager/master";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      agenix.url = "github:ryantm/agenix";

      tshm-plugin = {
        url = "https://registry.yarnpkg.com/typescript-tshm-plugin/-/typescript-tshm-plugin-0.1.0.tgz";
        flake = false;
      };
    };

  outputs = { nixpkgs, darwin, home-manager, agenix, flake-utils, tshm-plugin, ... }:
    # Only support Alakazam's system architecture for the timebeing.
    let
      selfpkgs =
        (
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs { inherit system; };
          in
          {
            bangin = pkgs.callPackage ./pkg/bangin.nix { };
            bangin-server-node = pkgs.callPackage ./pkg/bangin-server-node.nix { };
            bangup = pkgs.callPackage ./pkg/bangup { };
            corrupter = pkgs.callPackage ./pkg/corrupter.nix { };
            proton-ge = pkgs.callPackage ./pkg/proton-ge.nix { };
            qbpm = pkgs.callPackage ./pkg/qbpm.nix { };
            tofi = pkgs.callPackage ./pkg/tofi.nix { };
            tshm = pkgs.callPackage ./pkg/tshm.nix { };
          }
        ); in


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
        packages.${system} = selfpkgs;
      }
    ) //

    {
      nixosConfigurations = {
        alakazam =
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
              inherit system;

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
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
              }
              agenix.nixosModule
              ./hosts/alakazam
            ];

            specialArgs = {
              inherit system selfpkgs;
              uname = "sam";
              email = "hello@samhh.com";

              emailPassPath = "emails/migadu.com/mailbox/hello";
              nasPath = "/mnt/nas";

              inherit agenix;
              tshmPlugin = tshm-plugin;

              termBin = "${pkgs.foot}/bin/foot";
              launcherBin = "${selfpkgs.tofi}/bin/tofi";
              webBrowserBin = "${pkgs.qutebrowser}/bin/qutebrowser";
              streamerBin = "${pkgs.streamlink}/bin/streamlink";
            };
          };

        tentacool =
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
              inherit system;
            };
          in
          nixpkgs.lib.nixosSystem {
            inherit pkgs system;

            modules = [
              agenix.nixosModule
              ./hosts/tentacool
            ];

            specialArgs = {
              inherit system;
              uname = "sam";
              email = "hello@samhh.com";

              nasPath = "/mnt/nas";

              inherit agenix;
            };
          };
      };

      darwinConfigurations.lapras =
        let
          system = "aarch64-darwin";
          pkgs = import nixpkgs {
            inherit system;
          };
        in
        darwin.lib.darwinSystem {
          inherit system;

          modules = [
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
            }
            ./hosts/lapras
          ];

          specialArgs = {
            uname = "sam";
            email = "hello@samhh.com";
          };
        };
    };
}
