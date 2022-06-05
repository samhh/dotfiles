{
  inputs =
    {
      nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

      home-manager = {
        url = "github:nix-community/home-manager/master";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      tshm-plugin = {
        url = "https://registry.yarnpkg.com/typescript-tshm-plugin/-/typescript-tshm-plugin-0.1.0.tgz";
        flake = false;
      };
    };

  outputs = { nixpkgs, home-manager, tshm-plugin, ... }:
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
          in nixpkgs.lib.nixosSystem {
            inherit pkgs system;

            modules = [
              home-manager.nixosModules.home-manager {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
              }
              ./hosts/alakazam
            ];

            specialArgs = {
              uname = "sam";
              email = "hello@samhh.com";

              emailPassPath = "emails/migadu.com/mailbox/hello";
              nasPath = "/mnt/nas";

              tshmPlugin = tshm-plugin;

              termBin = "${pkgs.foot}/bin/foot";
              editorBin = "${pkgs.neovim}/bin/nvim";
              launcherBin = "${pkgs.dmenu}/bin/dmenu";
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
          in nixpkgs.lib.nixosSystem {
            inherit pkgs system;

            modules = [
              ./hosts/tentacool
            ];

            specialArgs = {
              uname = "sam";
              email = "hello@samhh.com";

              nasPath = "/mnt/nas";
            };
          };
      };
    };
}
