{ systems, ... }:

with systems; [
  {
    hostname = "alakazam";
    system = x86_64-linux;
    isNixOS = true;
    isHeadful = true;
  }

  {
    hostname = "tentacool";
    system = x86_64-linux;
    isNixOS = true;
    isHeadful = false;
  }

  {
    hostname = "lapras";
    system = aarch64-darwin;
    isNixOS = false;
    isHeadful = true;
  }
]
