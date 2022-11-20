{ pkgs, ... }:

pkgs.writeShellScriptBin "wallpaper" ''
  dir="$1"

  # The not path hides dotfiles and `@eaDir`.
  ${pkgs.findutils}/bin/find "$dir" -type f -not -path '*/[@.]*' | \
    ${pkgs.coreutils}/bin/shuf -n1 | \
    ${pkgs.findutils}/bin/xargs ${pkgs.swaybg}/bin/swaybg -m fill -i
''
