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
    sh = "nix shell nixpkgs#";
    up = "home-manager switch --flake ~/Dev/dotfiles/";
  };
}
