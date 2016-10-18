
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
   dotspacemacs-configuration-layer-path '("~/.spacemacs-extra/")
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
     go

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
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     (auto-completion :variables
                      ;;auto-completion-return-key-behavior nil
                      auto-completion-enable-snippets-in-popup t
                      ;;auto-completion-enable-help-tooltip t
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-sort-by-usage t
                      :disabled-for org)

     ;; ----MISCELANIOUS----
     colors
     git
     org
     osx
     ;; Improved file browsing with vim commands.
     (ranger :variables ranger-override-dired t)
     semantic
     ivy
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
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil

   ;; ------- START SCREEN -------
   ;; Banner = dogemacs bby.
   dotspacemacs-startup-banner 999
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-startup-recent-list-size 10
   dotspacemacs-scratch-mode 'text-mode

   ;; ------- SCHEMES / WINDOWS -------
   dotspacemacs-themes '(darktooth
                         gruvbox
                         doom-one
                         spacemacs-dark
                         )
   ;; Don't recolour cursor (T'is gui emacs only.)
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Slightly larger font than default (13-> 14)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight medium
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers 'relative
   dotspacemacs-highlight-delimiters 'all


   ;; ------- KEY BINDINGS -------
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is the equivalent of pressing `<leader> m`
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; Don't treat C-i/Tab as different keys. Only would work for GUI emacs anyway.
   dotspacemacs-distinguish-gui-tab nil
   ;; The command key used for Evil commands (ex-commands) and Emacs commands (M-x).
   dotspacemacs-command-key ";"
   dotspacemacs-remap-Y-to-y$ t

   ;; ------- WINDOW LAYOUTS -------
   dotspacemacs-default-layout-name "Default"
   ;; Don't display the default layout name in the mode-line.
   dotspacemacs-display-default-layout nil
   ;; LOOK INTO THIS: Last auto saved layouts are resumed automatically upon start.
   dotspacemacs-auto-resume-layouts nil

   ;; ------- SAVING / LOADING -------
   ;; Autosave files to cache.
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5

   ;; ------- PLUGIN CONFIG -------
   ;; Don't use ido.
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil

   dotspacemacs-which-key-delay 1.0
   dotspacemacs-which-key-position 'bottom

   dotspacemacs-smartparens-strict-mode nil

   ;; ------- SPACEMACS GENERAL STUFF -------
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; Delete trailing whitespace while saving buffer.
   dotspacemacs-whitespace-cleanup 'trailing
   ))

;; Is called immediately after `dotspacemacs/init', before layer configuration executes.
(defun dotspacemacs/user-init ())

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (push "~/.spacemacs-extra/" load-path)

  (setq-default

   tab-width 4
   evil-shift-width 4

   evil-shift-round nil

   vc-follow-symlinks t

   ;; Stops emacs creating .# files, which stop other programs editing stuff while emacs is doin it's stuff.
   create-lockfiles nil

   spacemacs-show-trailing-whitespace nil
   )

  ;; Wrap whole words in text documents, and by character in programming buffers.
  (spacemacs/toggle-truncate-lines-off)
  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  ;; Set the spellcheck language.
  (setq ispell-dictionary "english")
  ;; Make spellcheck work with special charachters.
  (add-to-list 'ispell-local-dictionary-alist
               (quote ("my_english" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8)))

  (require 'oc-keys)
  (require 'oc-modeline)
  ;; Calling this here means reloading config file still works.
  (setq-default mode-line-format (doom-mode-line))

  ;; JAVA
  (setq-default eclim-eclipse-dirs "~/Development/java-mars/Eclipse.app/"
                eclim-executable "~/Development/java-mars/Eclipse.app/Contents/Eclipse/eclim")

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
    "g"  'intero-goto-definition))


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
