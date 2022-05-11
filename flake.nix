{
  inputs =
    {
      nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

      home-manager = {
        url = "github:nix-community/home-manager/master";
        inputs.nixpkgs.follows = "nixpkgs";
      };
    };

  outputs = { nixpkgs, home-manager, ... }:
    let
      hostName = "alakazam";
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;

        config.allowUnfree = true;
      };

      lib = nixpkgs.lib;
    in {
      nixosConfigurations.${hostName} = lib.nixosSystem {
        inherit pkgs system;

        modules = [
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
          ./audio.nix
          ./backup.nix
          ./core.nix
          ./de.nix
          ./editor.nix
          ./email.nix
          ./gaming.nix
          ./hardware.nix
          ./hass.nix
          ./misc.nix
          ./network.nix
          ./rss.nix
          ./security.nix
          ./terminal.nix
          ./user.nix
          ./vcs.nix
          ./web.nix
        ];

        specialArgs = {
          inherit hostName;

          uname = "sam";
          email = "hello@samhh.com";

          emailPassPath = "emails/migadu.com/mailbox/hello";
          nasPath = "/mnt/nas";

          termBin = "${pkgs.foot}/bin/foot";
          editorBin = "${pkgs.neovim}/bin/nvim";
          launcherBin = "${pkgs.dmenu}/bin/dmenu";
          webBrowserBin = "${pkgs.qutebrowser}/bin/qutebrowser";
          streamerBin = "${pkgs.streamlink}/bin/streamlink";
        };
      };
    };
}
