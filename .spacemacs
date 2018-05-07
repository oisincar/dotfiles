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
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode ;; Open header files in c++ mode, rather than c mode.
            c-c++-enable-clang-support t)
     csharp
     csv
     elixir
     elm
     emacs-lisp
     go

     (haskell :variables
              haskell-completion-backend 'intero)

     html
     java
     javascript
     latex
     lua
     markdown
     python
     shell-scripts ;; layer for editing shell scripts
     yaml
     vimscript

     ;; ----SHELL----
     (shell :variables
            shell-default-shell 'eshell ;; ansi-term
            shell-default-height 40
            shell-default-position 'bottom
            )

     ;; ----TEXT ENTRY----
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking

     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-enable-snippets-in-popup t
                      ;;auto-completion-enable-help-tooltip t
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-sort-by-usage t
                      :disabled-for org)

     ;; ----MISCELANIOUS----
     colors
     git
      ;; Enables jump-to definition for many languages. Need to apt install package 'global'.
      ;; Should possibly switch to using ctags?
     gtags
     ivy
     org
     osx
     ;; Improved file browsing with vim commands.
     (ranger :variables ranger-override-dired t)
     semantic
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; dotspacemacs-additional-packages '(intero company-ghci)
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
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil

   ;; ------- START SCREEN -------
   ;; Banner = dogemacs bby.
   dotspacemacs-startup-banner 999
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-startup-recent-list-size 10
   dotspacemacs-scratch-mode 'text-mode

   ;; ------- SCHEMES / WINDOWS -------
   dotspacemacs-themes '(doom-one
                darktooth
                gruvbox
                )
   ;; Don't recolour cursor (T'is gui emacs only.)
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Slightly larger font than default (13-> 14)
   ;;dotspacemacs-default-font '("Droid sans mono"
   dotspacemacs-default-font '("Source Code Pro"
                               ;; :size 13
                               :size 22
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
   dotspacemacs-distinguish-gui-tab t
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
  (setq-default indent-tabs-mode nil)
  )

;; Configuration function for user code.
(defun dotspacemacs/user-config ()
  (push "~/.spacemacs-extra/" load-path)

  (setq-default
   ;; tab-width 4
   evil-shift-width 4
   c-default-style "bsd"
   c-basic-offset 4

   ;; evil-shift-round nil

   vc-follow-symlinks t

   ;; Stops emacs creating .# files, which stop other programs editing stuff while emacs is doin it's stuff.
   create-lockfiles nil

   spacemacs-show-trailing-whitespace nil
   )

  (setq python-indent 4)
  (setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-flake8-maximum-line-length 120)

  ;; Wrap whole words in text documents, and by character in programming buffers.
  (spacemacs/toggle-truncate-lines-off)
  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  ;; Default behavour is like mac term, tab cycles through possible completions.
  ;; (setq eshell-cmpl-cycle-completions nil)

  ;; Get eshell completion to use ivy
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<tab>")
                (lambda () (interactive) (completion-at-point)))))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "C-l")
                (lambda () (interactive) (completion-at-point)))))

  ;; Add headers to fix c++ headers file.
  ;;(add-to-list 'company-c-headers-path-system "/usr/include/c++/5.4.0/" )
  (defun setup-flycheck-clang-project-path ()
    (let ((root (ignore-errors (projectile-project-root))))
      (when root
        (add-to-list
         (make-variable-buffer-local 'flycheck-clang-include-path)
         root))))

  (add-hook 'c++-mode-hook 'setup-flycheck-clang-project-path)

	;; Temp indent with spaces.. Not sure why this is broken atm.
  (add-hook 'emacs-lisp-mode-hook
    (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'csharp-mode-hook
    (lambda () (setq indent-tabs-mode nil)))

  (require 'oc-keys)
  (require 'oc-modeline)
  ;; Calling this here means reloading config file doesn't reset bar.
  (setq-default mode-line-format (doom-mode-line))

  ;; Get ranger to show files with the proper emacs modes by default.
  (setq ranger-show-literal nil)
  ;; Don't preview binary filse or these extensions.
  (setq ranger-dont-show-binary t)
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
  ;; Don't preview files larger than 1MB.
  (setq ranger-max-preview-size 1)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(package-selected-packages
   (quote
    (ghub let-alist ggtags vimrc-mode dactyl-mode org-category-capture avy company f s yaml-mode csv-mode ob-elixir lua-mode yapfify xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit stickyfunc-enhance srefactor spaceline powerline smex smeargle slim-mode shell-pop scss-mode sass-mode reveal-in-osx-finder restart-emacs request ranger rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el pbcopy paradox spinner osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro alert log4e gntp org-download org-bullets open-junk-file omnisharp shut-up org-plus-contrib neotree multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc ivy-hydra intero insert-shebang info+ indent-guide hydra hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core haskell-snippets haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-ivy flyspell-correct flycheck-pos-tip pos-tip flycheck-mix flycheck-haskell flycheck-elm flycheck-credo flycheck flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight eshell-z eshell-prompt-extras esh-help emmet-mode elm-mode elisp-slime-nav dumb-jump disaster diminish cython-mode csharp-mode counsel-projectile projectile counsel swiper ivy company-web web-completion-data company-tern dash-functional tern company-shell company-go go-mode company-ghci company-ghc ghc haskell-mode company-emacs-eclim eclim company-cabal company-c-headers company-auctex company-anaconda column-enforce-mode color-identifiers-mode cmm-mode cmake-mode clean-aindent-mode clang-format bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed dash anaconda-mode pythonic alchemist elixir-mode pkg-info epl ace-link ac-ispell auto-complete popup doom-themes evil-unimpaired company-statistics coffee-mode auctex async aggressive-indent adaptive-wrap ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
	'(ansi-color-names-vector
		 ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
	'(package-selected-packages
		 (quote
			 (spotify ghub let-alist ggtags vimrc-mode dactyl-mode org-category-capture avy company f s yaml-mode csv-mode ob-elixir lua-mode yapfify xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit stickyfunc-enhance srefactor spaceline powerline smex smeargle slim-mode shell-pop scss-mode sass-mode reveal-in-osx-finder restart-emacs request ranger rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el pbcopy paradox spinner osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro alert log4e gntp org-download org-bullets open-junk-file omnisharp shut-up org-plus-contrib neotree multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc ivy-hydra intero insert-shebang info+ indent-guide hydra hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core haskell-snippets haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-ivy flyspell-correct flycheck-pos-tip pos-tip flycheck-mix flycheck-haskell flycheck-elm flycheck-credo flycheck flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight eshell-z eshell-prompt-extras esh-help emmet-mode elm-mode elisp-slime-nav dumb-jump disaster diminish cython-mode csharp-mode counsel-projectile projectile counsel swiper ivy company-web web-completion-data company-tern dash-functional tern company-shell company-go go-mode company-ghci company-ghc ghc haskell-mode company-emacs-eclim eclim company-cabal company-c-headers company-auctex company-anaconda column-enforce-mode color-identifiers-mode cmm-mode cmake-mode clean-aindent-mode clang-format bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed dash anaconda-mode pythonic alchemist elixir-mode pkg-info epl ace-link ac-ispell auto-complete popup doom-themes evil-unimpaired company-statistics coffee-mode auctex async aggressive-indent adaptive-wrap ace-window)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
