# Setting up enviroment…

### Clone .vimrc:

>git clone https://github.com/oisincar/vimrc.git

>ln ~/vimrc/.vimrc ~/.vimrc


Or something like that!

Next try not to kill yourself with sheer frustration aliasing stuff to their various folders with neovim. Or just use a vim that reads your .vimrc from root like normal!

### Clone vundle, and download all plugins:

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

Open vim, and run command :PluginInstall

### If ycm error run:

cd ~/.vim/bundle/YouCompleteMe
./install.py --clang-completer
