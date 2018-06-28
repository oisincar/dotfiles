# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"


# . /home/oisincar/torch/install/bin/torch-activate
# if [ -e /home/oisincar/.nix-profile/etc/profile.d/nix.sh ]; then . /home/oisincar/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
# 
# # Add opencv to libraries list.
# export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib

# Cuda/ tensorflow shtuff.
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda/lib64:/usr/local/cuda/extras/CUPTI/lib64"
export CUDA_HOME=/usr/local/cuda
