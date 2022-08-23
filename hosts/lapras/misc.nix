{ pkgs, uname, ... }:

{
  home-manager.users.${uname}.home.packages = with pkgs; [
    pass
    ripgrep
    shellcheck
  ];
}
