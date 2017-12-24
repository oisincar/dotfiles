alias tmux="TERM=screen-256color-bce tmux"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# virtualenv and virtualenvwrapper
export WORKON_HOME=/home/oisincar/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

# Git
alias gs='git status '
alias ga='git add '
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias gk='gitk --all&'

alias got='git '
alias get='git '

# Servers
alias sshcube='ssh cube.netsoc.tcd.ie'
alias sshspoon='ssh spoon.netsoc.tcd.ie'

# Utils
alias ll='ls -al'
alias vim='nvim'

# Create emacs eshell aliases file
alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" >~/.emacs.d/.cache/eshell/alias
