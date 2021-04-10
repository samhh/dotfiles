# Fish configuration
fish_vi_key_bindings
set fish_greeting

# Exported variables
## Explicitly define XDG defaults that are used below
set -x XDG_BIN_HOME ~/.local/bin/
set -x XDG_CONFIG_HOME ~/.config/
set -x XDG_DATA_HOME ~/.local/share/

## Respect XDG
set -x CARGO_HOME "$XDG_DATA_HOME/cargo"
set -x DOCKER_CONFIG "$XDG_CONFIG_HOME/docker"
set -x GHCUP_USE_XDG_DIRS true
set -x GOPATH "$XDG_DATA_HOME/go"
set -x GNUPGHOME "$XDG_CONFIG_HOME/gnupg/"
set -x GRADLE_USER_HOME "$XDG_DATA_HOME/gradle/"
set -x HISTFILE "$XDG_DATA_HOME/bash/history"
set -x LESSHISTFILE "$XDG_DATA_HOME/less/history"
set -x NODE_REPL_HISTORY "$XDG_DATA_HOME/node/history"
set -x PASSWORD_STORE_DIR "$XDG_DATA_HOME/pass/"
set -x RUSTUP_HOME "$XDG_DATA_HOME/rustup/"
set -x STACK_ROOT "$XDG_DATA_HOME/stack/"
set -x SQLITE_HISTORY "$XDG_DATA_HOME/sqlite/history"
set -x TS_NODE_HISTORY "$XDG_DATA_HOME/ts-node/history"
set -x WINEPREFIX "$XDG_DATA_HOME/wine/pfx/default/"
set -x XAUTHORITY "$XDG_RUNTIME_DIR/Xauthority"
set -x XINITRC "$XDG_CONFIG_HOME/init"

## Use nvim as editor wherever possible
set -x VISUAL nvim
set -x EDITOR $VISUAL

## Use nvim for man pages
set -x MANPAGER nvim +Man!

## Inform shell environment of preexisting ssh-agent socket
set -x SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"

## Use rootless Docker socket
set -x DOCKER_HOST "unix://$XDG_RUNTIME_DIR/docker.sock"

# Extend PATH for XDG & Cabal (which doesn't yet respect XDG)
fish_add_path "$XDG_BIN_HOME"
fish_add_path ~/.cabal/bin/

# Command drop-in replacement aliases
abbr vi 'nvim'
abbr top 'gotop'
abbr mpc 'vimpc'

# Arch package management-specific aliases
abbr in 'sudo pacman -S'
abbr ina 'paru -S'
abbr up 'sudo pacman -Syu'
abbr upa 'paru -Sau'
abbr un 'sudo pacman -Rs'

# Misc/specific aliases
abbr sys 'systemctl'
abbr sysu 'systemctl --user'

# Create directory path and cd into it
function mkcd
    mkdir -p $argv;
    cd $argv;
end

