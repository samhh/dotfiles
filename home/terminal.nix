{ pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    font = {
      package = pkgs.hasklig;
      name = "Hasklig Regular";
    };
    shellIntegration.mode = "no-cursor";
    settings = {
      remember_window_size = false;
      enabled_layouts = "fat, grid";
      visual_window_select_characters = "tnseriao";
      scrollback_pager_history_size = "10";
    };
    keybindings = {
      "cmd+w" = "discard_event";
      "cmd+t" = "launch --cwd=current";
      "cmd+l" = "next_layout";
      "cmd+e" = "focus_visible_window";
      "cmd+f" = "show_scrollback";
      "cmd+p" = "show_last_command_output";
    };
  };

  # Suppress login shell MOTD.
  home.file.".hushlogin".text = "";
}
