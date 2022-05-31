{ fetchurl, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton7-20";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha512 = "b3e88e4013b725ad90a3178e064cf038bd2b2afac30acaf963ff3d5240a68755700c26afb61393b71e34d51d5cef330a180206d4ac97cc603305e64c2b1cf7db";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';
}
