{ fetchurl, installShellFiles, lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "bangin";
  version = "0.2.1";

  src = fetchurl {
    url = "https://git.sr.ht/~samhh/bangin/archive/${version}.tar.gz";
    sha256 = "12a66vm0954bbq05q063k9273gjbd838qdyid0hmjmirbx7ydgia";
  };

  nativeBuildInputs = with pkgs; [
    installShellFiles
    makeWrapper
  ];

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/bin/
    mv bangin.sh $out/bin/bangin

    installManPage bangin.1
  '';

  fixupPhase = with pkgs; ''
    wrapProgram $out/bin/bangin \
      --set PATH ${lib.makeBinPath [ gawk ]}
  '';

  meta = {
    homepage = "https://sr.ht/~samhh/bangin/";
    description = "bangin is a primitive, portable shell script which enables DuckDuckGo-like bangs.";
    license = lib.licenses.mit;
  };
}
