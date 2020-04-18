# Fish configuration
fish_vi_key_bindings
set fish_greeting

# Exported variables
## Use Nvim as editor wherever possible
set -x VISUAL nvim
set -x EDITOR $VISUAL
## Use taskrc outside of home dir
set -x TASKRC ~/.config/task/config

# Source nodenv
status --is-interactive; and source (nodenv init -|psub)

# Generic aliases
abbr c 'clear'
abbr cdc 'cd; and clear'
abbr ... 'cd ../..'
abbr .... 'cd ../../..'
abbr ..... 'cd ../../../..'
abbr ...... 'cd ../../../../..'
abbr rmds 'find . -name "*.DS_Store" -type f -delete'

# Command drop-in replacement aliases
abbr rm 'trash'
abbr vi 'nvim'

# Arch package management-specific aliases
abbr up 'brew update; and brew upgrade; and fisher'
abbr clear_cache 'brew cu; and brew cleanup'

# Misc/specific aliases
abbr todo 'clear; and task todo'

# Create directory path and cd into it
function mkcd
    mkdir -p $argv;
    cd $argv;
end

