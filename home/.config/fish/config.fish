# Disable greeting
set fish_greeting

# vi-like keybinds
fish_vi_key_bindings

# fzf keybind
fzf_configure_bindings --directory=\cf

# Show hidden dirs in fzf
set -x fzf_fd_opts --hidden --exclude=.git

# Exported variables
## Non-XDG locations
set -x LEDGER_FILE ~/journal/main.journal
set -x PASSWORD_STORE_DIR ~/passwords

## Explicitly define XDG defaults that are used below
set -x XDG_BIN_HOME ~/.local/bin/
set -x XDG_CACHE_HOME ~/.cache/
set -x XDG_CONFIG_HOME ~/.config/
set -x XDG_DATA_HOME ~/.local/share/

## Respect XDG
set -x CARGO_HOME "$XDG_DATA_HOME/cargo"
set -x GHCUP_USE_XDG_DIRS true
set -x GOPATH "$XDG_DATA_HOME/go"
set -x GNUPGHOME "$XDG_CONFIG_HOME/gnupg/"
set -x GRADLE_USER_HOME "$XDG_DATA_HOME/gradle/"
set -x HISTFILE "$XDG_DATA_HOME/bash/history"
set -x LESSHISTFILE "$XDG_DATA_HOME/less/history"
set -x NODE_REPL_HISTORY "$XDG_DATA_HOME/node/history"
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

## Use nvim for pacdiff
set -x DIFFPROG nvim -d

## Inform shell environment of preexisting ssh-agent socket
set -x SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"

## Inform Nix of non-nixpkgs channels like Home Manager. See:
##   https://github.com/NixOS/nix/issues/2033
set -x NIX_PATH ~/.nix-defexpr/channels/

# Source Nix
bass source /etc/profile.d/nix-daemon.sh

# Source direnv for Nix
direnv hook fish | source

# Sync colorscheme
source "$XDG_CACHE_HOME/wal/colors.fish"
cat "$XDG_CACHE_HOME/wal/sequences"

# Extend PATH for XDG & Cabal (which doesn't yet respect XDG)
fish_add_path "$XDG_BIN_HOME"
fish_add_path ~/.cabal/bin/

# Command drop-in replacement aliases
abbr -g vi 'nvim'
abbr -g top 'gotop'
abbr -g mpc 'vimpc'

# Arch package management-specific aliases
abbr -g in 'doas pacman -S'
abbr -g ina 'paru -S'
abbr -g up 'doas pacman -Syu'
abbr -g upa 'paru -Sau'
abbr -g un 'doas pacman -Rs'

# Misc/specific aliases
abbr -g sys 'systemctl'
abbr -g sysu 'systemctl --user'
abbr -g getclip 'xclip -o -selection clipboard'
abbr -g setclip 'xclip -i -selection clipboard'

# Create directory path and cd into it
function mkcd
    mkdir -p $argv;
    cd $argv;
end

# Create file, recursively creating directories as needed
function mktouch
    mkdir -p (dirname $argv);
    touch $argv;
end
