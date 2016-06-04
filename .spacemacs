;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----LANGUAGES----
     c-c++
     csharp
     elixir
     elm
     emacs-lisp
     git
     go
     (haskell :variables
              haskell-enable-hindent-style "Andrew Gibiansky"
              haskell-enable-ghci-ng-support t
              ;; haskell-enable-shm-support t ;; <(doesn't work well with evil, but it's pretty sexy so.. Hm.)
              )
     html
     java
     javascript
     javascript
     latex
     markdown
     python
     ;;scala ;; Error with this, :/.

     ;; ----SHELL----
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-width 50
            shell-default-height 50
            shell-default-position 'left)
     shell-scripts

     ;; ----TEXT ENTRY----
     spell-checking
     syntax-checking
     (auto-completion :variables
                      ;;auto-completion-return-key-behavior nil
                      auto-completion-enable-snippets-in-popup t
                      ;;auto-completion-enable-help-tooltip t
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-sort-by-usage t
                      :disabled-for org erc)

     ;; ----MISCELANIOUS----
     org
     ranger  ;; Improved file browsing with vim commands.
     semantic
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Called at the very startup of Spacemacs initialization before layers configuration.
   You should not put any user code in here besides modifying the variable values."
  (setq-default
   ;; ------- PACKAGES -------
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   ;; Check for update upon startup. Might want to disable.
   dotspacemacs-check-for-update t
   ;; Best and only editing style.
   dotspacemacs-editing-style 'vim
   ;; Stuff shows up in messages bout loading.
   dotspacemacs-verbose-loading nil
   ;; Banner = dogemacs.

   ;; ------- START SCREEN -------
   dotspacemacs-startup-banner 999
   ;; List to show on startup.
   dotspacemacs-startup-lists '(recents projects bookmarks)
   ;; Number of recent files to show in the startup buffer.
   dotspacemacs-startup-recent-list-size 10
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; ------- SCHEMES / ASTETICS -------
   dotspacemacs-themes '(gruvbox
                         darktooth
                         spacemacs-dark
                         monokai)
   ;; Don't recolour cursor (T'is gui emacs only.)
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Slightly larger font than default (13-> 14)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; If non nil a progress bar is displayed when spacemacs is loading.
   ;; Set to nil to maybe boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; The transparency % of a frame when it's active or selected/ inactive.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; Smooth vim-like scrolling.
   dotspacemacs-smooth-scrolling t
   ;; Relative line numbers.
   dotspacemacs-line-numbers 'relative
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; ------- KEY BINDINGS -------
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is the equivalent of pressing `<leader> m`
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; Don't treat C-i/Tab as different keys. Only would work for GUI emacs anyway.
   dotspacemacs-distinguish-gui-tab nil
   ;; The command key used for Evil commands (ex-commands) and Emacs commands (M-x).
   dotspacemacs-command-key ";"
   ;; Remap y to y$.
   dotspacemacs-remap-Y-to-y$ t

   ;; ------- WINDOW LAYOUTS -------
   ;; Name of the default window layout.
   dotspacemacs-default-layout-name "Default"
   ;; Don't display the default layout name in the mode-line.
   dotspacemacs-display-default-layout nil
   ;; LOOK INTO THIS: Last auto saved layouts are resumed automatically upon start.
   dotspacemacs-auto-resume-layouts nil

   ;; ------- SAVING / LOADING -------
   ;; Autosave files to cache.
   ;; Set to 'nil to disable autosave, and 'origional to save in place.
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache.
   dotspacemacs-max-rollback-slots 5

   ;; ------- PLUGIN CONFIG -------
   ;; Don't use ido.
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source. (default nil)
   dotspacemacs-helm-no-header t
   ;; define the position to display `helm'.
   dotspacemacs-helm-position 'bottom
   ;; Disable paste-micro-state.
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds.
   dotspacemacs-which-key-delay 0.6
   ;; Which-key frame position.
   dotspacemacs-which-key-position 'bottom

   ;; ------- SPACEMACS GENERAL STUFF -------
   ;; Smartparens-strict-mode enforces ballenced parrens. Pretty annoying.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag")
   ;; Delete trailing whitespace while saving buffer.
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; Used for haskell mode.. I think.. Not sure if needed
  ;; (add-to-list 'exec-path "~/.local/bin")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Escape key binding.
  (setq-default evil-escape-key-sequence "qj")
  (setq-default evil-escape-delay 0.2)

  ;; Disable highlighting of trailing whitespace.
  (setq spacemacs-show-trailing-whitespace nil)

  ;; Split window vertically using | and horizontally with _
  (define-key evil-normal-state-map "|" 'split-window-right-and-focus)
  (define-key evil-normal-state-map "_" 'split-window-below-and-focus)

  ;; Copy entire buffer.. Soon. (" bc" does work as a binding)
  ;; (define-key evil-normal-state-map " bc" ')

  ;; Use deer instead of default directory search. Allows for more vim bindings.
  (add-hook 'makefile-mode-hook 'deer)

  ;; Instead of / doing regular search, use OP helm-swoop.
  (define-key evil-normal-state-map "/" 'helm-swoop)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
