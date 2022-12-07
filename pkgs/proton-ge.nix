{ fetchurl, lib, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton7-42";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha512 = "7f8679f325b134b5e539acc6115a2433fbee6668c30090a504fd8b319e8cf71baf36d6a38bba2a01e79453cdd167fa2b34c7f2393b1208dbbd0161fc14e146c5";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';

  meta.platforms = lib.platforms.linux;
}
