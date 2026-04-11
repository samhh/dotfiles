{ pkgs }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "amoxide";
  version = "0.6.0";

  src = pkgs.fetchFromGitHub {
    owner = "sassman";
    repo = "amoxide-rs";
    rev = "v${version}";
    hash = "sha256-dW4NeV35c8zpnVEvPDl4T6b7MPY+zJsBWZU+kF7IB0Y=";
  };

  cargoHash = "sha256-ylwuKQ8GH9jFa3nO8N5ofSm4Wk8t2jpCA6OXuexMJ0g=";

  meta = {
    description = "Shell alias manager — manage aliases globally via profiles or per-project";
    homepage = "https://github.com/sassman/amoxide-rs";
    mainProgram = "am";
  };
}
