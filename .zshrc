# Enable tab completion
autoload -Uz compinit
compinit

# Save input history
HISTFILE=~/.histfile
HISTSIZE=250
SAVEHIST=250

# Prevent saving the same history twice in a row
setopt histignoredups

# Emacs mode bindings
bindkey -e

# Enable changing directory without cd
setopt autocd

# Enable matching dotfiles in globs without specifying dot
setopt globdots

# Use nano as default cli editor
export VISUAL=nano
export EDITOR="$VISUAL"

# Current dir as iTerm tab title
precmd() {
  echo -ne "\e]1;${PWD##*/}\a"
}

# Enable compatibility with Bash completions
autoload -Uz bashcompinit
bashcompinit

# Yarn
# Negligible addition to startup time
export PATH="$PATH:$HOME/.yarn/bin"

# autojump
# Adds ~150ms startup time
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

# nvm
# Do not install nvm via Homebrew as that adds an additional ~500ms startup time
# Alternative lazy load startup from: https://www.reddit.com/r/node/comments/4tg5jg/lazy_load_nvm_for_faster_shell_start/d5ib9fs/
# Negligible addition to startup time with lazy loading
# Adds at least ~1200ms startup time without lazy loading
declare -a NODE_GLOBALS=(`find ~/.nvm/versions/node -maxdepth 3 -type l -wholename '*/bin/*' | xargs -n1 basename | sort | uniq`)

NODE_GLOBALS+=("node")
NODE_GLOBALS+=("nvm")

load_nvm () {
  export NVM_DIR=~/.nvm
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
}

for cmd in "${NODE_GLOBALS[@]}"; do
  eval "${cmd}(){ unset -f ${NODE_GLOBALS}; load_nvm; ${cmd} \$@ }"
done

# Configure minimal theme (to be loaded by zplug)
MINIMAL_PWD_CHAR_LEN=25

# Enable zplug and desired plugins/themes
# Adds ~400ms startup time
export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"
zplug "djui/alias-tips"
zplug "supercrabtree/k"
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

# Bind zsh-history-substring-search to up and down arrow keys in iTerm
bindkey "$terminfo[cuu1]" history-substring-search-up
bindkey "$terminfo[cud1]" history-substring-search-down

# Helper function aliases
# cd to the path of the front Finder window
cdf() {
  target=`osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)'`
  if [ "$target" != "" ]; then
    cd "$target"; pwd
  else
    echo 'No Finder window found' >&2
  fi
}

# Generic aliases
alias rmds='find . -name "*.DS_Store" -type f -delete' # recursively remove .DS_Store
alias rmall='rm -rf * && rm .*'
alias npmlsg='npm ls -g --depth=0' # npm global packages
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias cdgit='cd-gitroot'
alias code='subl .'
alias vi='nvim'
alias nvmup='nvm install node --reinstall-packages-from=node'
alias gitrmmerged='git br --merged | grep -Ev "(\*|master|develop)" | xargs -n 1 git br -d'
alias clean='brew cleanup -s'
unalias run-help

# Impero aliases
alias dokku='bash $HOME/.dokku/contrib/dokku_client.sh'
alias stg='DOKKU_HOST=stg.paas.impero.me dokku'
alias prd='DOKKU_HOST=prd.paas.impero.me dokku'
