{ fetchurl, lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "tofi";
  version = "0.6.0";

  src = fetchurl {
    url = "https://github.com/philj56/tofi/archive/refs/tags/v${version}.tar.gz";
    sha256 = "05jh65klqzbgszgyq6a1cllskk54acibkvz2xvnqnyxf0b9dv8v5";
  };

  buildInputs = with pkgs; [ cairo harfbuzz libxkbcommon meson ninja pango pkg-config scdoc wayland wayland-protocols ];

  meta.platforms = lib.platforms.linux;
}
