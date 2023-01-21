{ fetchurl, lib, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton7-47";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha512 = "29fbab0865ec4aaa472a6777b88f5378e166550cca8814f66c10d22b022367848ce4db895575f6792c375fb4bfbacb8e97900511b243531572d4f9f49fd9700f";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';

  meta.platforms = lib.platforms.linux;
}
