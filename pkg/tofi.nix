{ fetchurl, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "tofi";
  version = "v0.5.0";

  src = fetchurl {
    url = "https://github.com/philj56/tofi/archive/refs/tags/${version}.tar.gz";
    sha256 = "0n7i1ks4fmwkzyn68na7lxdw0smgq3pkzaz89mnaji1ig0k8k0b8";
  };

  buildInputs = with pkgs; [ cairo harfbuzz libxkbcommon meson ninja pango pkg-config scdoc wayland wayland-protocols ];
}
