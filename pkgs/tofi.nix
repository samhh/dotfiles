{ fetchurl, lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "tofi";
  version = "0.7.0";

  src = fetchurl {
    url = "https://github.com/philj56/tofi/archive/refs/tags/v${version}.tar.gz";
    sha256 = "1kb3ivbvn4ryd1m8a7zcr8xc0flgvyya4wbdyqigcd14ffxq0imq";
  };

  buildInputs = with pkgs; [ cairo harfbuzz libxkbcommon meson ninja pango pkg-config scdoc wayland wayland-protocols ];

  meta.platforms = lib.platforms.linux;
}
