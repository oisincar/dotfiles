
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
     git
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





  (funcall (function setup-modeline))
  )

(defun setup-modeline ()

  (defun doom-fix-unicode (font &rest chars)
    "Display certain unicode characters in a specific font.
  e.g. (doom-fix-unicode \"DejaVu Sans\" ?⚠ ?★ ?λ)"
    (declare (indent 1))
    (mapc (lambda (x) (set-fontset-font
                       t (cons x x)
                       (cond ((fontp font)
                              font)
                             ((listp font)
                              (font-spec :family (car font) :size (nth 1 font)))
                             ((stringp font)
                              (font-spec :family font))
                             (t (error "FONT is an invalid type: %s" font)))))
          chars))


  (defun doom/project-root (&optional strict-p)
    "Get the path to the root of your project."
    (let (projectile-require-project-root strict-p)
      (projectile-project-root)))

  (powerline-reset)
  (spaceline-compile)

  ;;; core-modeline.el

  ;; This file tries to be an almost self-contained configuration of my mode-line.
  ;;
  ;; It depends on the following external packages:
  ;;   + REQUIRED
  ;;       + f
  ;;       + s
  ;;       + powerline
  ;;       + projectile
  ;;       + DejaVu Mono for Powerline font <https://github.com/powerline/fonts>
  ;;   + OPTIONAL
  ;;       + evil-mode
  ;;       + anzu + evil-anzu
  ;;       + iedit and evil-multiedit
  ;;       + flycheck
  ;;
  ;; The only external functions used are:
  ;;  `doom-fix-unicode'  in core/core-defuns.el
  ;;  `doom/project-root' in core/defuns/defuns-projectile.el
  ;;
  ;; Both are simple, isolated functions and, besides projectile, has no other
  ;; dependencies.

  (defvar mode-line-height 15 ;; 30
    "How tall the mode-line should be. This is only respected in GUI emacs.")

  ;; Load powerline only when uncompiled, in order to generate the xpm bitmaps for
  ;; the mode-line. This is the tall blue bar on the left of the mode-line.
  ;; NOTE Compile this file for a faster startup!
  (eval-when-compile (require 'powerline))
  ;; FIXME Don't hardcode colors in
  (defvar mode-line-bar          (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil)))
  (defvar mode-line-eldoc-bar    (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil)))
  (defvar mode-line-inactive-bar (eval-when-compile (pl/percent-xpm 10 100 0 100 0 3 nil nil)))

  ;; Custom faces
  (defface mode-line-is-modified nil
    "Face for mode-line modified symbol")

  (defface mode-line-2 nil
    "The alternate color for mode-line text.")

  ;; (defface mode-line-highlight nil
  ;;; Edited by me.
  (defface mode-line-highlight '((,c (:foreground ,black :background ,yellow)))
    "Face for bright segments of the mode-line.")

  (defface mode-line-count-face nil
    "Face for anzu/evil-substitute/evil-search number-of-matches display.")

  ;; Git/VCS segment faces
  (defface mode-line-vcs-info '((t (:inherit warning)))
    "")
  (defface mode-line-vcs-warning '((t (:inherit warning)))
    "")

  ;; Flycheck segment faces
  (defface doom-flycheck-error '((t (:inherit error)))
    "Face for flycheck error feedback in the modeline.")
  (defface doom-flycheck-warning '((t (:inherit warning)))
    "Face for flycheck warning feedback in the modeline.")


  ;;
  ;; Functions
  ;;

  (defun doom-ml-flycheck-count (state)
    "Return flycheck information for the given error type STATE."
    (when (flycheck-has-current-errors-p state)
      (if (eq 'running flycheck-last-status-change)
          "?"
        (cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

  ;; pyenv/rbenv version segment
  (defvar doom-ml-env-version-hook '()
    "Hook that runs whenever the environment version changes (e.g. rbenv/pyenv)")

  (defun doom-ml|env-update ()
    (when doom-ml--env-command
      (let ((default-directory (doom/project-root)))
        (let ((s (shell-command-to-string doom-ml--env-command)))
          (setq doom-ml--env-version (if (string-match "[ \t\n\r]+\\'" s)
                                      (replace-match "" t t s)
                                    s))
          (run-hook-with-args 'doom-ml-env-version-hook doom-ml--env-version)))))

  (defmacro def-version-cmd! (modes command)
    "Define a COMMAND for MODE that will set `doom-ml--env-command' when that mode is
  activated, which should return the version number of the current environment. It is used
  by `doom-ml|env-update' to display a version number in the modeline. For instance:

    (def-version-cmd! ruby-mode \"ruby --version | cut -d' ' -f2\")

  This will display the ruby version in the modeline in ruby-mode buffers. It is cached the
  first time."
    (add-hook! (focus-in find-file) 'doom-ml|env-update)
    `(add-hook! ,modes (setq doom-ml--env-command ,command)))


  ;;
  ;; Initialization
  ;;

  ;; Where (py|rb)env version strings will be stored
  (defvar-local doom-ml--env-version nil)
  (defvar-local doom-ml--env-command nil)

  ;; Make certain unicode glyphs bigger for the mode-line.
  ;; FIXME Replace with all-the-icons?
  (doom-fix-unicode '("DejaVu Sans Mono" 15) ?✱) ;; modified symbol
  (let ((font "DejaVu Sans Mono for Powerline"))
    (doom-fix-unicode (list font 12) ?)  ;; git symbol
    (doom-fix-unicode (list font 16) ?∄)  ;; non-existent-file symbol
    (doom-fix-unicode (list font 15) ?)) ;; read-only symbol

  ;; So the mode-line can keep track of "the current window"
  (defvar mode-line-selected-window nil)
  (defun doom|set-selected-window (&rest _)
    (let ((window (frame-selected-window)))
      (when (and (windowp window)
                 (not (minibuffer-window-active-p window)))
        (setq mode-line-selected-window window))))
  (add-hook 'window-configuration-change-hook #'doom|set-selected-window)
  (add-hook 'focus-in-hook #'doom|set-selected-window)
  (advice-add 'select-window :after 'doom|set-selected-window)
  (advice-add 'select-frame  :after 'doom|set-selected-window)


  ;;
  ;; Mode-line segments
  ;;

  (defun *buffer-path ()
    "Displays the buffer's full path relative to the project root (includes the
  project root). Excludes the file basename. See `*buffer-name' for that."
    (when buffer-file-name
      (propertize
       (f-dirname
        (let ((buffer-path (file-relative-name buffer-file-name (doom/project-root)))
              (max-length (truncate (/ (window-body-width) 1.75))))
          (concat (projectile-project-name) "/"
                  (if (> (length buffer-path) max-length)
                      (let ((path (reverse (split-string buffer-path "/" t)))
                            (output ""))
                        (when (and path (equal "" (car path)))
                          (setq path (cdr path)))
                        (while (and path (<= (length output) (- max-length 4)))
                          (setq output (concat (car path) "/" output))
                          (setq path (cdr path)))
                        (when path
                          (setq output (concat "../" output)))
                        (when (string-suffix-p "/" output)
                          (setq output (substring output 0 -1)))
                        output)
                    buffer-path))))
       'face (if active 'mode-line-2))))

  (defun *buffer-name ()
    "The buffer's base name or id."
    ;; FIXME Don't show uniquify tags
    (s-trim-left (format-mode-line "%b")))

  (defun *buffer-pwd ()
    "Displays `default-directory', for special buffers like the scratch buffer."
    (propertize
     (concat "[" (abbreviate-file-name default-directory) "]")
     'face 'mode-line-2))

  (defun *buffer-state ()
    "Displays symbols representing the buffer's state
  (non-existent/modified/read-only)"
    (when buffer-file-name
      (propertize
       (concat (if (not (file-exists-p buffer-file-name))
                   "∄"
                 (if (buffer-modified-p) "✱"))
               (if buffer-read-only ""))
       'face 'mode-line-is-modified)))

  (defun *buffer-encoding-abbrev ()
    "The line ending convention used in the buffer."
    (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
        ""
      (symbol-name buffer-file-coding-system)))

  (defun *major-mode ()
    "The major mode, including process, environment and text-scale info."
    (concat (format-mode-line mode-name)
            (if (stringp mode-line-process) mode-line-process)
            (if doom-ml--env-version (concat " " doom-ml--env-version))
            (and (featurep 'face-remap)
                 (/= text-scale-mode-amount 0)
                 (format " (%+d)" text-scale-mode-amount))))

  (defun *vc ()
    "Displays the current branch, colored based on its state."
    (when vc-mode
      (let ((backend (concat " " (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))
            (face (let ((state (vc-state buffer-file-name)))
                    (cond ((memq state '(edited added))
                           'mode-line-vcs-info)
                          ((memq state '(removed needs-merge needs-update conflict removed unregistered))
                           'mode-line-vcs-warning)))))
        (if active
            (propertize backend 'face face)
          backend))))

  (defvar-local doom--flycheck-err-cache nil "")
  (defvar-local doom--flycheck-cache nil "")
  (defun *flycheck ()
    "Persistent and cached flycheck indicators in the mode-line."
    (when (and (featurep 'flycheck)
               flycheck-mode
               (or flycheck-current-errors
                   (eq 'running flycheck-last-status-change)))
      (or (and (or (eq doom--flycheck-err-cache doom--flycheck-cache)
                   (memq flycheck-last-status-change '(running not-checked)))
               doom--flycheck-cache)
          (and (setq doom--flycheck-err-cache flycheck-current-errors)
               (setq doom--flycheck-cache
                     (let ((fe (doom-ml-flycheck-count 'error))
                           (fw (doom-ml-flycheck-count 'warning)))
                       (concat
                        (if fe (propertize (format " •%d " fe)
                                           'face (if active
                                                     'doom-flycheck-error
                                                   'mode-line)))
                        (if fw (propertize (format " •%d " fw)
                                           'face (if active
                                                     'doom-flycheck-warning
                                                   'mode-line))))))))))

  (defun *selection-info ()
    "Information about the current selection, such as how many characters and
  lines are selected, or the NxM dimensions of a block selection."
    (when (and active (evil-visual-state-p))
      (propertize
       (let ((reg-beg (region-beginning))
             (reg-end (region-end))
             (evil (eq 'visual evil-state)))
         (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
               (chars (- (1+ reg-end) reg-beg))
               (cols (1+ (abs (- (evil-column reg-end)
                                 (evil-column reg-beg))))))
           (cond
            ;; rectangle selection
            ((or (bound-and-true-p rectangle-mark-mode)
                 (and evil (eq 'block evil-visual-selection)))
             (format " %dx%dB " lines (if evil cols (1- cols))))
            ;; line selection
            ((or (> lines 1) (eq 'line evil-visual-selection))
             (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
                 (format " %dL " lines)
               (format " %dC %dL " chars lines)))
            (t (format " %dC " (if evil chars (1- chars)))))))
       'face 'mode-line-highlight)))

  (defun *macro-recording ()
    "Display current macro being recorded."
    (when (and active defining-kbd-macro)
      (propertize
       (format " %s ▶ " (char-to-string evil-this-macro))
       'face 'mode-line-highlight)))

  (make-variable-buffer-local 'anzu--state)
  (defun *anzu ()
    "Show the current match number and the total number of matches. Requires anzu
  to be enabled."
    (when (and (featurep 'evil-anzu) (evil-ex-hl-active-p 'evil-ex-search))
      (propertize
       (format " %s/%d%s "
               anzu--current-position anzu--total-matched
               (if anzu--overflow-p "+" ""))
       'face (if active 'mode-line-count-face))))

  (defun *evil-substitute ()
    "Show number of :s matches in real time."
    (when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
      (propertize
       (let ((range (if evil-ex-range
                        (cons (car evil-ex-range) (cadr evil-ex-range))
                      (cons (line-beginning-position) (line-end-position))))
             (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
         (if pattern
             (format " %s matches "
                     (count-matches pattern (car range) (cdr range))
                     evil-ex-argument)
           " ... "))
       'face (if active 'mode-line-count-face))))

  (defun *iedit ()
    "Show the number of iedit regions matches + what match you're on."
    (when (and (boundp 'iedit-mode) iedit-mode)
      (propertize
       (let ((this-oc (let (message-log-max) (iedit-find-current-occurrence-overlay)))
             (length (or (ignore-errors (length iedit-occurrences-overlays)) 0)))
         (format
          " %s/%s "
          (save-excursion
            (unless this-oc
              (iedit-prev-occurrence)
              (setq this-oc (iedit-find-current-occurrence-overlay)))
            (if this-oc
                ;; NOTE: Not terribly reliable
                (- length (-elem-index this-oc iedit-occurrences-overlays))
              "-"))
          length))
       'face (if active 'mode-line-count-face))))

  (defun *buffer-position ()
    "A more vim-like buffer position."
    (let ((start (window-start))
          (end (window-end))
          (pend (point-max)))
      (if (and (= start 1)
               (= end pend))
          ":All"
        (cond ((= start 1) ":Top")
              ((= end pend) ":Bot")
              (t (format ":%d%%%%" (/ end 0.01 pend)))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun doom-mode-line (&optional id)
    `(:eval
      (let* ((active (eq (selected-window) mode-line-selected-window))
             (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
                        (*flycheck)
                        (*macro-recording)
                        (*selection-info)
                        (*anzu)
                        (*evil-substitute)
                        (*iedit)
                        " "
                        ;; (*buffer-path)
                        (*buffer-name)
                        " "
                        (*buffer-state)
                        ,(if (eq id 'scratch) '(*buffer-pwd))))
             (rhs (list (*buffer-encoding-abbrev)
                        (*vc)
                        "  " (*major-mode) "  "
                        (propertize
                         (concat "(%l,%c) " (*buffer-position))
                         'face (if active 'mode-line-2))))
             (middle (propertize
                      " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                         ,(1+ (string-width (format-mode-line rhs)))))))))
        (list lhs middle rhs))))

  (setq-default mode-line-format (doom-mode-line))
  )
  ;;; core-modeline.el ends here

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
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
