# Fish configuration
fish_vi_key_bindings
set fish_greeting

# Exported variables
set -x VISUAL nvim
set -x EDITOR $VISUAL

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

# Autostart Xorg
if status is-login
  if test -z "$DISPLAY" -a $XDG_VTNR = 1
    exec startx -- -keeptty
  end
end

