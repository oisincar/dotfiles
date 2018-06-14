# Setting up enviroment…

As long as you're feeling brave..
Simply move everything to be in the home directory on linux.
```
git clone <this dir> ~/dotfiles/
mv ~/dotfiles/{.,}* ~
```

On windows .spacemacs needs to go elsewhere. Depends on emacs install loc.

## Install doom on windows...

```
path %path%;"C:\Users\Oisin Carroll\Downloads\emacs-w64-25.3-O2-with-modules\emacs\bin"
emacs -q --eval "(setq user-emacs-directory default-directory load-prefer-newer t)" --batch -l init.el -f doom//FUNCTION
```

Where FUNCTION is
```
reload-autoloads
packages-autoremove
packages-install
byte-compile -- -r
```
