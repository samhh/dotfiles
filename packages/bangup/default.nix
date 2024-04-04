{
  lib,
  pkgs,
  stdenv,
}:

stdenv.mkDerivation rec {
  pname = "bangup";
  version = "0.0.0";

  src = ./bangup.sh;

  nativeBuildInputs = with pkgs; [ makeWrapper ];

  dontUnpack = true;

  installPhase = ''
    mkdir -p $out/bin/
    cp $src $out/bin/bangup
  '';

  fixupPhase = with pkgs; ''
    wrapProgram $out/bin/bangup \
      --set PATH ${
        lib.makeBinPath [
          coreutils
          curl
        ]
      }
  '';
}
