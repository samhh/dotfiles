{ ... }:

{
  programs.home-manager.enable = true;

  home = rec {
    stateVersion = "23.05";
    username = "sam";
    homeDirectory = "/Users/${username}";
  };
}
