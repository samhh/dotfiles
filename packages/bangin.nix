{ fetchurl, installShellFiles, lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "bangin";
  version = "0.2.1";

  src = fetchurl {
    url = "https://git.sr.ht/~samhh/bangin/archive/${version}.tar.gz";
    sha256 = "16r9ncsfxaa0s10fym559wq285m0fzhvirpzxi74ggahw73fx51d";
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
