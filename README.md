# Setting up enviroment…

### Clone .vimrc:

git clone https://github.com/oisincar/vimrc.git ~/

### Clone vundle, and download all plugins:

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

Open vim, and run command :PluginInstall

### If ycm error run:

cd ~/.vim/bundle/YouCompleteMe
./install.py --clang-completer
