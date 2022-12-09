{ pkgs, ... }:

{
  system.stateVersion = "22.05";

  nix.settings.auto-optimise-store = true;

  environment.systemPackages = with pkgs; [ git ];
}
