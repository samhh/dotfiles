{ config, ... }:

let
  # Unmanaged so that agents can edit it. AGENTS.md has no standard global
  # location, so link it into each tool's own.
  instructions = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.agents/AGENTS.md";
in
{
  home.file.".claude/CLAUDE.md".source = instructions;
  home.file.".codex/AGENTS.md".source = instructions;
  home.file.".pi/agent/AGENTS.md".source = instructions;
  xdg.configFile."delta/AGENTS.md".source = instructions;
  xdg.configFile."zed/AGENTS.md".source = instructions;
}
