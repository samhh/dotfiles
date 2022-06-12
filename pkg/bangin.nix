{ fetchurl, lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "bangin";
  version = "0.2.0";

  src = fetchurl {
    url = "https://git.sr.ht/~samhh/bangin/archive/${version}.tar.gz";
    sha256 = "lSY6LY009kYOid5DX3yiL2XQemYeYxtGWLLCd47AsQw=";
  };

  nativeBuildInputs = with pkgs; [
    makeWrapper
  ];

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/bin/
    mv bangin.sh $out/bin/bangin
  '';

  fixupPhase = with pkgs; ''
    wrapProgram $out/bin/bangin \
      --set PATH ${lib.makeBinPath [ gnused ]}
  '';

  meta = {
    homepage = "https://sr.ht/~samhh/bangin/";
    description = "bangin is a primitive, portable shell script which enables DuckDuckGo-like bangs.";
    license = lib.licenses.mit;
  };
}
