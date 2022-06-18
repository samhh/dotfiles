{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default =
        pkgs.haskellPackages.developPackage {
          root = ./.;
          # Needed for HLS to work inside `nix develop` (after a `cabal build`).
          modifier = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
            cabal-install
          ]);
        };

      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/os";
      };
    };
}
