{ systems, ... }:

with systems; [
  {
    hostname = "alakazam";
    system = x86_64-linux;
    isHeadful = true;
    config = ./alakazam;
  }

  {
    hostname = "tentacool";
    system = x86_64-linux;
    isHeadful = false;
    config = ./tentacool;
  }
]
