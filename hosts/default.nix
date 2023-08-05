{ systems, ... }:

with systems; [
  {
    hostname = "alakazam";
    system = x86_64-linux;
    config = ./alakazam;
  }

  {
    hostname = "tentacool";
    system = x86_64-linux;
    config = ./tentacool;
  }
]
