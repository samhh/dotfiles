# Fish configuration
fish_vi_key_bindings
set fish_greeting

# Exported variables
## Use nvim as editor wherever possible
set -x VISUAL nvim
set -x EDITOR $VISUAL

## Use nvim for man pages
set -x MANPAGER nvim +Man!

## Inform shell environment of preexisting ssh-agent socket
set -x SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"

## Use rootless Docker socket
set -x DOCKER_HOST "unix://$XDG_RUNTIME_DIR/docker.sock"

## Don't store less pager history
set -x LESSHISTFILE /dev/null

# Extend PATH for ghcup and Cabal
set -a fish_user_paths ~/.ghcup/bin/ ~/.cabal/bin/

# Command drop-in replacement aliases
abbr vi 'nvim'
abbr top 'gotop'
abbr mpc 'vimpc'

# Arch package management-specific aliases
abbr in 'sudo pacman -S'
abbr ina 'sudo aura -A'
abbr up 'sudo pacman -Syu; and sudo aura -Auk'
abbr un 'sudo pacman -Rs'

# Misc/specific aliases
abbr s 'systemctl'

# Create directory path and cd into it
function mkcd
    mkdir -p $argv;
    cd $argv;
end

