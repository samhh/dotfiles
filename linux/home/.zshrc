# Autostart Xorg
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  startx
fi

# Enable tab completion
autoload -Uz compinit
compinit

# Save input history
HISTFILE=~/.histfile
HISTSIZE=250
SAVEHIST=250

# Prevent saving the same history twice in a row
setopt histignoredups

# Vi mode keybindings
bindkey -v
export KEYTIMEOUT=1

# Enable changing directory without cd
setopt autocd

# Enable matching dotfiles in globs without specifying dot
setopt globdots

# Use Neovim as default cli editor
export VISUAL=nvim
export EDITOR="$VISUAL"

# Enable compatibility with Bash completions
autoload -Uz bashcompinit
bashcompinit

# Set terminal title dynamically in supported xterms
# From: https://wiki.archlinux.org/index.php/zsh#xterm_title
autoload -Uz add-zsh-hook

function xterm_title_precmd () {
  print -Pn '\e]2;%n@%m %2~\a'
}

function xterm_title_preexec () {
  print -Pn '\e]2;%n@%m %2~ %# '
  print -n "${(q)1}\a"
}

if [[ "$TERM" == (screen*|xterm*|rxvt*) ]]; then
  add-zsh-hook -Uz precmd xterm_title_precmd
  add-zsh-hook -Uz preexec xterm_title_preexec
fi

# Rust/Cargo
export PATH="$PATH:$HOME/.cargo/bin"

# Yarn
export PATH="$PATH:$HOME/.yarn/bin"

# autojump
source /etc/profile.d/autojump.sh

# nvm
source /usr/share/nvm/init-nvm.sh

# Configure minimal theme (to be loaded by zplug)
MINIMAL_PWD_CHAR_LEN=25

# Enable zplug and desired plugins/themes
export ZPLUG_HOME=/usr/share/zsh/scripts/zplug
source $ZPLUG_HOME/init.zsh

zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
# Disabled due to [rip]grep alias error, see:
# https://github.com/djui/alias-tips/issues/40
# zplug "djui/alias-tips"
zplug "mollifier/cd-gitroot"
zplug "samhh/minimal-colorful-git-status"
zplug "subnixr/minimal"
if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi
zplug load

# Fix incompatibility between minimal theme and zsh-autosuggestions
ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=minimal-magic-enter

# Bind zsh-history-substring-search to up and down arrow keys
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Aliases
alias c='clear'
alias cdc='cd && clear'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

alias rmds='find . -name "*.DS_Store" -type f -delete' # recursively remove .DS_Store
alias rmall='rm -rf * && rm .*'

alias nvmup='nvm install node --reinstall-packages-from=node'
alias npmlsg='npm ls -g --depth=0' # npm global packages

alias cdgit='cd-gitroot'
alias gitrmmerged='git br --merged | grep -Ev "(\*|master|develop)" | xargs -n 1 git br -d'

alias setclip='xclip -selection c'
alias getclip='xclip -selection clipboard -o'

alias up='pacaur -Syu'
alias upf='pacaur -Syyu'
alias in='pacaur -S'
alias un='pacaur -Rs'

# Fix SSH environments
# See: https://unix.stackexchange.com/a/67541
alias ssh='TERM=xterm ssh'
#
# Hack to make sudo use aliases always
alias sudo='sudo '

# nvim is aliased via neovim-drop-in AUR package
alias ls='exa'
alias cat='ccat'
alias grep='rg -uu'
alias rm='trash-put'

