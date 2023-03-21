{ fetchurl, lib, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton7-51";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha512 = "466de3c00283c0e7ddd59bbb0d905548784ae678c077e9a872837ed79b6274923aa3ffcd3a237bb06b4bb79843f29ae7fa6c98932bd28aea1a056dd252c45eaa";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';

  meta.platforms = lib.platforms.linux;
}
