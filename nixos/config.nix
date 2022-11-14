{ pkgs, ... }:

{
  apps = {
    terminal.bin = "${pkgs.foot}/bin/foot";
    launcher.bin = "${pkgs.tofi}/bin/tofi";
    webBrowser.bin = "${pkgs.qutebrowser}/bin/qutebrowser";
    streamer.bin = "${pkgs.mpv}/bin/mpv";
  };
}
