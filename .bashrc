# Keep lots of history
export HISTSIZE=10000
export HISTFILESIZE=20000

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

# Utils
alias ll='ls -al'

alias god='~/Devcrap/WizLiz/godot/bin/godot.x11.opt.tools.64.mono'

# Create emacs eshell aliases file
#alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" >~/.emacs.d/.cache/eshell/alias

# Cuda/ tensorflow shtuff.
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda/lib64:/usr/local/cuda/extras/CUPTI/lib64"
export CUDA_HOME=/usr/local/cuda

# Bump up key repeat speed by a good bit (xset r rate #delay #rate).
xset r rate 200 40

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/oisincar/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/oisincar/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/oisincar/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/oisincar/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

