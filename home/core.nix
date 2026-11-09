{ ... }:

{
  programs.home-manager.enable = true;

  home = rec {
    stateVersion = "26.05";
    username = "sam";
    homeDirectory = "/Users/${username}";
  };

  xdg.enable = true;
}
