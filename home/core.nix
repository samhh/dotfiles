{ ... }:

{
  programs.home-manager.enable = true;

  home = rec {
    stateVersion = "23.05";
    username = "sam";
    homeDirectory = "/Users/${username}";
  };

  xdg.enable = true;

  programs.fish.shellAbbrs = {
    nixh = "nix-prefetch-url";
    nixhu = "nix-prefetch-url --unpack";
    sh = "nix shell nixpkgs#";
    up = "home-manager switch --flake ~/Dev/dotfiles/";
  };
}
