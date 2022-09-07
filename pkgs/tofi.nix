{ fetchurl, lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "tofi";
  # Latest release doesn't yet contain --require-match or --prompt-padding.
  version = "adc683b7fa847bc1776e3bece532c39aff12b039";

  src = fetchurl {
    url = "https://github.com/philj56/tofi/archive/${version}.tar.gz";
    sha256 = "192g1qwhnfhqcmmzv08frdkqwwi7z40nc6axhplvcmgn0mp524z9";
  };

  buildInputs = with pkgs; [ cairo harfbuzz libxkbcommon meson ninja pango pkg-config scdoc wayland wayland-protocols ];

  meta.platforms = lib.platforms.linux;
}
