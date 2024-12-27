{ ... }:

{
  xdg.configFile."ghostty/config".text = ''
    background-blur-radius = 20
    background-opacity = 0.92
    cursor-style-blink = false
    font-size = 12
    keybind = global:super+enter=toggle_quick_terminal
    quick-terminal-animation-duration = 0
    quick-terminal-position = center
    shell-integration-features = no-cursor
    theme = light:catppuccin-latte,dark:catppuccin-mocha
    window-padding-x = 8
  '';
}
