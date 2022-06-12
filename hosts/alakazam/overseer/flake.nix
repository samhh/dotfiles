{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
  };

  outputs = { self, nixpkgs, ... }:
    let system = "x86_64-linux"; in
    {
      packages.${system}.default =
        nixpkgs.legacyPackages.${system}.haskellPackages.developPackage {
          root = ./.;
        };

      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/os";
      };
    };
}
