{ ... }:

{
  programs.ghostty = {
    enable = true;
    package = null;
    settings = {
      background-blur = 20;
      background-opacity = 0.92;
      keybind = "global:super+`=toggle_visibility";
    };
  };
}
