# Detect OS (Linux or macOS) for later
if [[ $(uname) == 'Linux' ]]; then
  export OS=linux
elif [[ $(uname) == 'Darwin' ]]; then
  export OS=osx
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

# Set iTerm tab title to: `prevDir/currDir (command)`
precmd() {
  # number of characters to appear before truncation from the left
  rlength="20"

  tab_label="$PWD:h:t/$PWD:t"

  echo -ne "\e]1;${(l:rlength:)tab_label}\a"
}

# Enable compatibility with Bash completions
autoload -Uz bashcompinit
bashcompinit

# Pass completions
source /usr/local/etc/bash_completion.d/pass

# Rust/Cargo
export PATH="$PATH:$HOME/.cargo/bin"

# Yarn
export PATH="$PATH:$HOME/.yarn/bin"

# autojump
if [[ $OS == 'linux' ]]; then
  source /etc/profile.d/autojump.sh
elif [[ $OS == 'osx' ]]; then
  [ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh
fi

# nvm
# Do not install nvm via Homebrew as that adds an additional ~500ms startup time
# "--no-use" disables nvm until you explicitly call it with "nvm use", helping startup time. Can alternatively lazy load (look online for scripts)
if [[ $OS == 'linux' ]]; then
  source /usr/share/nvm/init-nvm.sh
elif [[ $OS == 'osx' ]]; then
  export NVM_DIR=~/.nvm
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" --no-use
fi

# Configure minimal theme (to be loaded by zplug)
MINIMAL_PWD_CHAR_LEN=25

# Enable zplug and desired plugins/themes
if [[ $OS == 'linux' ]]; then
  export ZPLUG_HOME=/usr/share/zsh/scripts/zplug
elif [[ $OS == 'osx' ]]; then
  export ZPLUG_HOME=/usr/local/opt/zplug
fi
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

# Generic aliases
alias c='clear'
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

# OS-specific aliases
if [[ $OS == 'linux' ]]; then
  alias setclip='xclip -selection c'
  alias getclip='xclip -selection clipboard -o'
elif [[ $OS == 'osx' ]]; then
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
fi

# Default replacement aliases
alias vi='nvim'
alias _vi='command vi'

alias ls='exa'
alias _ls='command ls'

alias cat='ccat'
alias _cat='command cat'

alias grep='rg -uu'
alias _grep='command grep'
