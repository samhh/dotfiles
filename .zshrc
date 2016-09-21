# The following lines were added by compinstall
zstyle :compinstall filename '/Users/samhh/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=250
SAVEHIST=250
bindkey -e
# End of lines configured by zsh-newuser-install

# Enable Antigen and desired plugins
source $(brew --prefix)/share/antigen/antigen.zsh
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle djui/alias-tips
antigen bundle supercrabtree/k
antigen theme S1cK94/minimal minimal
antigen apply

# Configure zsh-history-substring-search
zmodload zsh/terminfo
bindkey "$terminfo[cuu1]" history-substring-search-up
bindkey "$terminfo[cud1]" history-substring-search-down

# Current dir as iTerm tab title
precmd() {
  echo -ne "\e]1;${PWD##*/}\a"
}

# Open current path in Finder window
alias f='open -a Finder ./'

# cd to the path of the front Finder window
cdf() {
	target=`osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)'`
	if [ "$target" != "" ]; then
		cd "$target"; pwd
	else
		echo 'No Finder window found' >&2
	fi
}

# autojump
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

# nvm
export NVM_DIR="$HOME/.nvm"
. "$(brew --prefix nvm)/nvm.sh"

# recursively remove .DS_Store
alias rmds='find . -name "*.DS_Store" -type f -delete'

# Impero deployment helpers
alias dokku='$HOME/.dokku/contrib/dokku_client.sh'
alias prw='DOKKU_HOST=prd.prw.paas.impero.me dokku'
alias stg='DOKKU_HOST=stg.paas.impero.me dokku'
alias prd='DOKKU_HOST=prd.paas.impero.me dokku'
