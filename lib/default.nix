{ lib, ... }:

with lib; {
  compose = flip pipe;
}
