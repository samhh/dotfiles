# Fish configuration
fish_vi_key_bindings
set fish_greeting

# Exported variables
set -x VISUAL nvim
set -x EDITOR $VISUAL
set -x TASKRC ~/.config/task/config

# Generic aliases
alias c='clear'
alias cdc='cd; and clear'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias rmds='find . -name "*.DS_Store" -type f -delete'

# Command drop-in replacement aliases
alias grep='rg -uu'
alias rm='trash'
alias vi='nvim'

# App-specific (excl/ package management) aliases
alias npmlsg='npm ls -g --depth=0'

# Arch package management-specific aliases
alias up='brew update; and brew upgrade; and fisher'
alias clear_cache='brew cu; and brew cleanup'

# Use shell environment in sudo so that sudo knows the above aliases
alias sudo='sudo -sE'

