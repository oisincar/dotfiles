# Setting up enviroment…

_Clone .vimrc:_
git clone https://github.com/oisincar/vimrc.git ~/

_Clone vundle, and download all plugins:_
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
Open vim, and run command :PluginInstall

_If ycm error run:_
cd ~/.vim/bundle/YouCompleteMe
./install.py --clang-completer
