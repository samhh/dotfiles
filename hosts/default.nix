{ systems, ... }:

with systems; [
  {
    hostname = "alakazam";
    system = x86_64-linux;
    isNixOS = true;
    isHeadful = true;
    config = ./alakazam;
  }

  {
    hostname = "tentacool";
    system = x86_64-linux;
    isNixOS = true;
    isHeadful = false;
    config = ./tentacool;
  }

  {
    hostname = "lapras";
    system = aarch64-darwin;
    isNixOS = false;
    isHeadful = true;
    config = ./lapras;
  }
]
