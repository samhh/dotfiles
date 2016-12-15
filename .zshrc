# Enable tab completion
autoload -Uz compinit
compinit

# Save input history
HISTFILE=~/.histfile
HISTSIZE=250
SAVEHIST=250

# Emacs mode bindings
bindkey -e

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

# LastPass CLI Bash completions
source /usr/local/Cellar/lastpass-cli/1.0.0/share/bash-completion/completions/lpass

# Yarn
export PATH="$PATH:$HOME/.yarn/bin"

# autojump
# adds ~150ms startup time
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

# nvm
# do not install nvm via Homebrew as that adds an additional ~500ms startup time
# alternative lazy load startup from: https://www.reddit.com/r/node/comments/4tg5jg/lazy_load_nvm_for_faster_shell_start/d5ib9fs/
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
# adds ~400ms startup time
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
zplug load

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

# Impero aliases
alias dokku='bash $HOME/.dokku/contrib/dokku_client.sh'
alias stg='DOKKU_HOST=stg.paas.impero.me dokku'
alias prd='DOKKU_HOST=prd.paas.impero.me dokku'
