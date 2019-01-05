gx in normal mode for evil-exchange. Swap two words.

C-Space to collapse/ uncollapse stuff is amazing!!

Tab to jump to matching brace.. Pretty nice!

M-f for Swiper w/ submenu thingy is pretty cool.

Add some full screen/ focus command.

C-l to autocomplete in insertmode
C-h to open documentation in company (autocomplete).

plantUML looks interesting - Layer for making uml diagrams.
Check out 'write' layer, as well as latex stuff.

SPC t s for spellcheck.

SPC RET for bookmarks.. Might be interesting to learn.

Look at notes section more. (Learn org!)

Find out what :m is !

Possibly improve surround and swipe binds. Currently capital S for surronud, but
only in visual select mode. Missing 'sw' commands.

Todo 'hl-todo' looks interesting too!

TODO: Fix C-hjkl in terminal. Switch to eshell again mayyybe?

Learn workspaces! Workflows might include having several open for one project..
Different parts maybe.


Learn eshell commands!

  (defun +eshell|init-keymap ()
    "Setup eshell keybindings. This must be done in a hook because eshell-mode
redefines its keys every time `eshell-mode' is enabled."
    (when (featurep 'evil)
      (evil-define-key* 'normal eshell-mode-map
        [return]   #'+eshell/goto-end-of-prompt
        "c"        #'+eshell/evil-change
        "C"        #'+eshell/evil-change-line
        "d"        #'+eshell/evil-delete
        "D"        #'+eshell/evil-delete-line)
      (evil-define-key* 'insert eshell-mode-map
        [tab]      #'+eshell/pcomplete
        "\C-j"     #'evil-window-down
        "\C-k"     #'evil-window-up
        "\C-h"     #'evil-window-left
        "\C-l"     #'evil-window-right
        "\C-d"     #'+eshell/quit-or-delete-char
        "\C-p"     #'eshell-previous-input
        "\C-n"     #'eshell-next-input))
    (define-key! eshell-mode-map
      (kbd "C-s")   #'+eshell/search-history
      (kbd "C-c s") #'+eshell/split-below
      (kbd "C-c v") #'+eshell/split-right
      (kbd "C-c x") #'+eshell/kill-and-close
