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

# Set iTerm tab title to: `prevDir/currDir (command)`
precmd() {
  # number of characters to appear before truncation from the left
  rlength="20"

  tab_label="$PWD:h:t/$PWD:t"

  echo -ne "\\e]1;${(l:rlength:)tab_label}\\a"
}

# Enable compatibility with Bash completions
autoload -Uz bashcompinit
bashcompinit

# Rust/Cargo
export PATH="$PATH:$HOME/.cargo/bin"

# Yarn
export PATH="$PATH:$HOME/.yarn/bin"

# autojump
[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh

# nvm
# Do not install nvm via Homebrew as that adds an additional ~500ms startup time
# "--no-use" disables nvm until you explicitly call it with "nvm use", helping startup time. Can alternatively lazy load (look online for scripts)
export NVM_DIR=~/.nvm
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" --no-use

# Configure minimal theme (to be loaded by zplug)
MINIMAL_PWD_CHAR_LEN=25

# Enable zplug and desired plugins/themes
export ZPLUG_HOME=/usr/local/opt/zplug
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

alias setclip='pbcopy'
alias getclip='pbpaste'

alias brewup='brew update && brew upgrade && brew cu && brew cleanup'

# cd to the path of the front Finder window
cdf() {
  target=$(osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)')
  if [ "$target" != "" ]; then
    cd "$target" || return; pwd
  else
    echo 'No Finder window found' >&2
  fi
}

# Hack to make sudo use aliases always
alias sudo='sudo '

alias vi='nvim'
alias ls='exa'
alias cat='ccat'
alias grep='rg -uu'
alias rm='trash'

