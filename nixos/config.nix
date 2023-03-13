{ pkgs, ... }:

{
  apps = {
    terminal.bin = "${pkgs.foot}/bin/foot";
    launcher.bin = "${pkgs.tofi}/bin/tofi";
    webBrowser.bin = "${pkgs.librewolf}/bin/librewolf";
  };
}
