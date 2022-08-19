{ fetchurl, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton7-29";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha512 = "b4c7a6894302a302a15b6f3968987c98190461e9cbb4afa7738fe28fb24b1a2730140c424efc60a77624c8867bcc52e3b6ae1a60c9246aedf208ddb994d602d5";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';
}
