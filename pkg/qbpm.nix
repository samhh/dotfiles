# Based upon but not entirely identical to:
#   https://github.com/pvsr/qbpm/blob/60ae8dd44f300810fd5b50f0240872193af4ce37/default.nix

{ fetchurl, pkgs }:

with pkgs.python38Packages;
buildPythonPackage rec {
  pname = "qbpm";
  version = "0.6";
  src = fetchurl {
    url = "https://github.com/pvsr/qbpm/archive/refs/tags/${version}.tar.gz";
    sha256 = "1p465g9p5xhjrbh2v4w39h0k4jdnrrx4xkpkrf686knn1r86k2vw";
  };
  doCheck = true;
  SETUPTOOLS_SCM_PRETEND_VERSION = version;
  nativeBuildInputs = [ setuptools-scm ];
  propagatedBuildInputs = [ pyxdg ];
  checkInputs = [ pytest ];
  postInstall = ''
    mkdir -p $out/share/fish/vendor_completions.d
    cp completions/qbpm.fish $out/share/fish/vendor_completions.d/
  '';
}
