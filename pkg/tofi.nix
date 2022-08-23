{ fetchurl, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "tofi";
  # Latest release doesn't yet contain --require-match or --prompt-padding.
  version = "e72f81799b1184d6b1b1bf8f70ec32d9cc139556";

  src = fetchurl {
    url = "https://github.com/philj56/tofi/archive/${version}.tar.gz";
    sha256 = "1j5n6cmcyqgwbri4w50gid7v6p66fdddfna1g3iclrc1yqnzbyhz";
  };

  buildInputs = with pkgs; [ cairo harfbuzz libxkbcommon meson ninja pango pkg-config scdoc wayland wayland-protocols ];
}
