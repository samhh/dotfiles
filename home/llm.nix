# Ollama must be started manually on boot:
# $ ollama serve & disown

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ollama
  ];
}
