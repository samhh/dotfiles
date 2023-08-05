{ pkgs, ... }:

{
  username = "sam";
  fullName = "Sam A. Horvath-Hunt";
  email.address = "hello@samhh.com";
  nas = {
    path = "/mnt/nas";
    hiddenFiles = [ "@eaDir" ".DS_Store" ];
  };
  apps = {
    terminal.bin = "${pkgs.foot}/bin/foot";
    webBrowser.bin = "${pkgs.librewolf}/bin/librewolf";
  };
}
