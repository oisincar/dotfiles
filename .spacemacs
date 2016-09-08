
;;
;;    ____  _     _       _
;;   / __ \(_)   (_)     ( )
;;  | |  | |_ ___ _ _ __ |/ ___     ___ _ __   __ _  ___ ___ _ __ ___   __ _  ___ ___
;;  | |  | | / __| | '_ \  / __|   / __| '_ \ / _` |/ __/ _ \ '_ ` _ \ / _` |/ __/ __|
;;  | |__| | \__ \ | | | | \__ \  _\__ \ |_) | (_| | (_|  __/ | | | | | (_| | (__\__ \
;;   \____/|_|___/_|_| |_| |___/ (_)___/ .__/ \__,_|\___\___|_| |_| |_|\__,_|\___|___/
;;                                     | |
;;                                     |_|
;;

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

     ;; (haskell :variables
     ;;          haskell-enable-hindent-style "Andrew Gibiansky"
     ;;          haskell-enable-ghci-ng-support t
     ;;          ;; haskell-enable-shm-support t ;; <(doesn't play well with evil, but it's pretty sexy so.. Hm.)
     ;;          )

      ;haskell
     (haskell :variables
              haskell-enable-ghc-mod-support nil
              ;; haskell-enable-ghci-ng-support t
              haskell-process-type 'stack-ghci
              haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--with-ghc=intero")
              hindent-style  "johan-tibell"
              haskell-stylish-on-save t)

     html
     java
     javascript
     latex
     markdown
     python
     ;;scala ;; Error with this, :/.

     ;; ----SHELL----
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-height 40
            shell-default-position 'bottom)
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
                      :disabled-for org)

     ;; ----MISCELANIOUS----
     org
     ranger  ;; Improved file browsing with vim commands.
     semantic
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(intero company-ghci)
   ;; dotspacemacs-additional-packages '()
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

   ;; ------- START SCREEN -------
   ;; Banner = dogemacs bby.
   dotspacemacs-startup-banner 999
   ;; List to show on startup.
   dotspacemacs-startup-lists '(recents projects bookmarks)
   ;; Number of recent files to show in the startup buffer.
   dotspacemacs-startup-recent-list-size 10
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; ------- SCHEMES / WINDOWS -------
   dotspacemacs-themes '(gruvbox
                         darktooth
                         spacemacs-dark
                         monokai)
   ;; Don't recolour cursor (T'is gui emacs only.)
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Slightly larger font than default (13-> 14)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight medium
                               :width normal
                               :powerline-scale 1.1)
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t

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


;; Is called immediately after `dotspacemacs/init', before layer configuration executes.
(defun dotspacemacs/user-init ()
  (setq-default
  ;; Theoretically changes the default mode when terminal spawns.. But it's not working :/
  ;; (evil-set-initial-state 'term-mode 'emacs)


  ;; Ranger.
  ;; Use deer instead of dired in all cases.
   ranger-override-dired t

   ))

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

  ;; Split window vertically using | and horizontally with _
  (define-key evil-normal-state-map "|" 'split-window-right-and-focus)
  (define-key evil-normal-state-map "_" 'split-window-below-and-focus)

  ;; Java stuff..
  (setq eclim-eclipse-dirs "~/Development/java-mars/Eclipse.app/"
        eclim-executable "~/Development/java-mars/Eclipse.app/Contents/Eclipse/eclim")
  ;; Instead of / doing regular search, use OP helm-swoop.
  (define-key evil-normal-state-map "/" 'helm-swoop)

  ;; Auto correct word under cursor.
  (evil-leader/set-key "TAB" 'flyspell-auto-correct-word)

  ;; Since we stole next buffer from spc-tab, put it on SPC-$.. Because why not.
  (evil-leader/set-key "$" 'last-buffer)

  ;; Disable highlighting of trailing whitespace.
  (setq spacemacs-show-trailing-whitespace nil)

  ;; Tab width = 4, not 2.
  (setq-default tab-width 4)

  ;; Stops emacs creating .# files, which stop other programs editing stuff while emacs is doin it's stuff.
  (setq create-lockfiles nil)

  ;; Regular line numbers in insert mode, relative in normal.
  (add-hook 'evil-normal-state-entry-hook 'linum-relative-on)
  (add-hook 'evil-normal-state-exit-hook 'linum-relative-off)

  ;; TODO: Change the way terminal buffers center on the active line. It's kinda off putting for em to center even in 'normal-mode'
  ;; There's a way to do it in the spacemacs youtube video/ that guys config.


  ;; CSHARP
  (setq-default omnisharp-server-executable-path "~/Development/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")


  ;; HASKELL
  (add-hook 'haskell-mode-hook (lambda ()
          (message "haskell-mode-hook")
          (intero-mode)
          (push '(company-ghci :with company-yasnippet :with company-dabbrev) company-backends-haskell-mode)
          (interactive-haskell-mode)
          (turn-on-haskell-indentation)
          (hindent-mode)
          (setq haskell-stylish-on-save t) ;; override haskell layer
          ))

  (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
    "ht" 'haskell-process-do-type
    "l"  'hayoo
    "t"  'intero-type-at
    "T"  'spacemacs/haskell-process-do-type-on-prev-line
    "r"  'haskell-process-load-file
    "i"  'intero-info
    "I"  'haskell-do-info
    "g"  'intero-goto-definition)
    )

(defun haskell-do-info (cPos cEnd)
  "Bring repl and do :info under the current cursor word"
  (interactive "r")
  (let (inputStr oldPos endSymbol)
    ;; grab the string
    (setq oldPos cPos)
    (setq endSymbol (cdr (bounds-of-thing-at-point 'symbol)))
    (skip-syntax-backward "^(, ")
    (setq inputStr (buffer-substring-no-properties (point) endSymbol))
    (goto-char oldPos)
    (haskell-interactive-switch)
    (haskell-interactive-mode-run-expr (format ":info %s" inputStr))
    (haskell-interactive-switch-back)
   ))


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
