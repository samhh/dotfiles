{ lib, pkgs, ... }:

let
  # proton-ge = pkgs.callPackage /home/sam/nix/pkg/proton-ge.nix {};
  uname = "sam";
in {
  programs.steam.enable = true;

  home-manager.users.${uname} = {
    # # Courtesy of:
    # #   https://github.com/NixOS/nixpkgs/issues/73323#issuecomment-968274771
    # home.activation.proton-ge = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    #   target="${lib.xdgData}/Steam/compatibilitytools.d/Proton-${pkgs.proton-ge.version}"
    #   if ! [ -d "$target" ]; then
    #     cp -R ${pkgs.proton-ge} "$target"
    #     chmod -R u+w "$target"
    #   fi
    # '';

    home.packages = with pkgs; [
      # proton-ge
      mangohud
    ];
  };
}
