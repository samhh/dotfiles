{ config, lib, pkgs, ... }:

let
  dir = "${config.nas.path}/wallpapers/static";
  excludes =
    let f = x: "-not -path '*/" + x + "*'";
    in lib.concatStringsSep " " (map f config.nas.hiddenFiles);
in
pkgs.writeShellScriptBin "wallpaper" ''
  ${pkgs.findutils}/bin/find "${dir}" -type f ${excludes} | \
    ${pkgs.coreutils}/bin/shuf -n1 | \
    ${pkgs.findutils}/bin/xargs ${pkgs.swaybg}/bin/swaybg -m fill -i
''
