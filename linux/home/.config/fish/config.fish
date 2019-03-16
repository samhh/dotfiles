# Fish configuration
fish_vi_key_bindings
set fish_greeting

# Exported variables
## Use Nvim as editor wherever possible
set -x VISUAL nvim
set -x EDITOR $VISUAL
## Inform shell environment of preexisting ssh-agent socket
set -x SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"
## Use taskrc outside of home dir
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
alias rm='trash-put'
alias vi='nvim'
alias top='htop'

# App-specific (excl/ package management) aliases
alias npmlsg='npm ls -g --depth=0'

# Arch package management-specific aliases
alias up='yay -Syu; and fisher'
alias upf='yay -Syyu'
alias in='yay -S'
alias un='yay -Rs'
alias clear_cache='paccache -r; and yay -S -c'

# Use shell environment in sudo so that sudo knows the above aliases
alias sudo='sudo -s'

