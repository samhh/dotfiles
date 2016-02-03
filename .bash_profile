# Create text file backup of all file/dir names on external drive into documents (likely only temporary)
alias treebackup="tree /Volumes/{EXTERNAL DRIVE NAME HERE}/ > ~/Documents/passport-tree.txt"

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

# Remove cruft from terminal (username etc) - possibly overridden by the git bash thing below
export PS1="\W \$ "

# cwd as iterm tab title
if [ $ITERM_SESSION_ID ]; then
  export PROMPT_COMMAND='echo -ne "\033]0;${PWD##*/}\007"'
fi

# bash-git-prompt
if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
    source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
fi

# autojump
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh
