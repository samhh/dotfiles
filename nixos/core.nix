{ pkgs, ... }:

{
  nix.settings.auto-optimise-store = true;

  environment.systemPackages = with pkgs; [ git ];
}
