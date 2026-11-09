# dotfiles

Configuring the universe.

Over the years I've run various different operating systems, window managers, and development setups. The current incarnation reflects my desire for straightforward agentic development across devboxes, hence only a handful of plain, starter config files copied to machines as needed.

Anything more can be wired up on demand by agents. It's a brave new world.

## Setup

Copy the desired files to `$XDG_CONFIG_HOME`.

Suppress the terminal's last-login message:

```console
$ touch ~/.hushlogin
```

If using Fish, remember to set it as the login shell.
