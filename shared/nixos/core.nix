{ pkgs, ... }:

{
  system.stateVersion = "22.05";

  environment.systemPackages = with pkgs; [ git ];
}
