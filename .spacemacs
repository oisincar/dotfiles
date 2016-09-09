
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
     org
     ;; Improved file browsing with vim commands.
     (ranger :variables ranger-override-dired t)
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
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil

   ;; ------- START SCREEN -------
   ;; Banner = dogemacs bby.
   dotspacemacs-startup-banner 999
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-startup-recent-list-size 10
   dotspacemacs-scratch-mode 'text-mode

   ;; ------- SCHEMES / WINDOWS -------
   dotspacemacs-themes '(gruvbox
                         darktooth
                         spacemacs-dark
                         monokai)
   ;; Don't recolour cursor (T'is gui emacs only.)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Slightly larger font than default (13-> 14)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
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
   dotspacemacs-smooth-scrolling t
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
(defun dotspacemacs/user-init ()
  (setq-default
   ))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (setq-default

   evil-escape-key-sequence "qj"
   evil-escape-delay 0.2

   tab-width 4
   evil-shift-round nil

   vc-follow-symlinks t

   ;; Stops emacs creating .# files, which stop other programs editing stuff while emacs is doin it's stuff.
   create-lockfiles nil

   spacemacs-show-trailing-whitespace nil
   )

  ;; Split window vertically using | and horizontally with _
  (define-key evil-normal-state-map "|" 'split-window-right-and-focus)
  (define-key evil-normal-state-map "_" 'split-window-below-and-focus)

  ;; swap ` and ', since `'s the useful one.
  (define-key evil-normal-state-map "`" 'evil-goto-mark-line)
  (define-key evil-normal-state-map "'" 'evil-goto-mark)

  ;; Make backspace comment out stuff.
  (define-key evil-normal-state-map (kbd "DEL") 'evilnc-comment-or-uncomment-lines)

  ;; Make C-w delete word even if autocompletion window is active.
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Correcting speeellling mistakes.
  (evil-leader/set-key "TAB" 'flyspell-auto-correct-word)
  ;; Correct next incorrect spelling with a list to choose from.
  (setq ispell-following-word t)
  (define-key evil-normal-state-map "S" 'helm-flyspell-correct)
  ;; (define-key evil-normal-state-map "S" 'ispell-word)

  ;; Since we stole next buffer from spc-tab, put it on SPC-$.. Because why not.
  (evil-leader/set-key "$" 'last-buffer)

  ;; Wrap whole words in text documents, and by character in programming buffers.
  (spacemacs/toggle-truncate-lines-off)
  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)


  ;; Regular line numbers in insert mode, relative in normal.
  ;(add-hook 'evil-normal-state-entry-hook 'linum-relative-on)
  ;(add-hook 'evil-normal-state-exit-hook 'linum-relative-off)

  ;; TODO: Change the way terminal buffers center on the active line. It's kinda off putting for em to center even in 'normal-mode'
  ;; There's a way to do it in the spacemacs youtube video/ that guys config.

  ;; Java stuff..
  (setq eclim-eclipse-dirs "~/Development/java-mars/Eclipse.app/"
        eclim-executable "~/Development/java-mars/Eclipse.app/Contents/Eclipse/eclim")

  ;; JAVA
  (setq eclim-eclipse-dirs "~/Development/java-mars/Eclipse.app/"
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
    "g"  'intero-goto-definition)


  ;; Disabling keys I don't use...
  (evil-leader/set-key "ad" nil) ;; Deer

  (evil-leader/set-key "bm" nil) ;; Buffer move is the same as window move afaik.

  (evil-leader/set-key "ct" nil) ;; Usless commenting hotkeys
  (evil-leader/set-key "cT" nil) ;; Note: Need to find better home for comment binding.
  (evil-leader/set-key "cy" nil) ;; Backspace perhaps?
  (evil-leader/set-key "cY" nil)

  (evil-leader/set-key "fs" nil) ;; Save file
  (evil-leader/set-key "ft" nil) ;; Neotree toggle, ranger's better.

  (evil-leader/set-key "J" nil) ;; 'split sexp'

  (evil-leader/set-key "R" nil) ;; pcre2el (Translator between emacs regex and other syntaxes.. Or something.)

  ;; Search has some stuff to take out... Gotta figure out the best first doe..

  (evil-leader/set-key "u" nil) ;; Universal argument, allows for numeric motions with emacs bindings.

  (evil-leader/set-key "ws" nil) ;; Window split bindings (that i've remaped)
  (evil-leader/set-key "wS" nil)
  (evil-leader/set-key "wv" nil)
  (evil-leader/set-key "wV" nil)
  (evil-leader/set-key "w-" nil)
  (evil-leader/set-key "ww" nil) ;; Move to other window.

  (evil-leader/set-key "y" nil) ;; Don't need avy-line with relative line-no.
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
