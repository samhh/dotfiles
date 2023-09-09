{ fetchurl, lib, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton8-14";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha512 = "6936de58d5d61b04968562225fbc5fabd27d09869fe27daefe2a271b7a2fb4acea043430cbdebfe4589396ea8a01d4f80355a33227b662f049060059173fb348";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';

  meta.platforms = lib.platforms.linux;
}
