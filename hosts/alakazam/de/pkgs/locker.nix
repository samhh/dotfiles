{ pkgs, ... }:

pkgs.writeShellScriptBin "locker" ''
  img="/tmp/lock.png"

  ${pkgs.sway-contrib.grimshot}/bin/grimshot save screen - | ${pkgs.corrupter}/bin/corrupter - > "$img"
  ${pkgs.swaylock}/bin/swaylock -i "$img" "$@"
''
