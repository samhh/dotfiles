{ pkgs, ... }:

{
  noop = pkgs.runCommand "noop" { meta.mainProgram = "noop"; } "mkdir $out";
}
