;;; private/default/+bindings.el -*- lexical-binding: t; -*-




;; Unset unused keys!
(global-unset-key (kbd "C-j")) ;;inserts newline at point
(global-unset-key (kbd "C-o")) ;;inserts newline at point

(after! evil-escape
       (setq evil-escape-key-sequence "qj")
       (setq evil-escape-unordered-key-sequence t)
       )

;; expand-region's prompt can't tell what key contract-region is bound to, so we
;; tell it explicitly.
(setq expand-region-contract-fast-key "V")

(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag
      [remap newline]          #'newline-and-indent

      ;; Ensure there are no conflicts
      :nmvo doom-leader-key nil
      :nmvo doom-localleader-key nil

      ;; --- Global keybindings ---------------------------
      ;; Make M-x available everywhere
      :gnvime "M-x"  #'execute-extended-command
      :gnvime "A-x"  #'execute-extended-command

      ;; A little sandbox to run code in
      :gnvime "M-;"  #'eval-expression
      :gnvime "M-:"  #'doom/open-scratch-buffer

      ;; Text-scaling
      :ne "C-+"      (λ! (text-scale-set 0))
      :ne "C-="      #'text-scale-increase
      :ne "C--"      #'text-scale-decrease

      ;; Simple window/frame navigation/manipulation
      :ne "C-`"      #'+popup/toggle
      :ne "C-~"      #'+popup/raise
      :ne "M-t"      #'+workspace/new
      :ne "M-T"      #'+workspace/display
      :ne "M-w"      #'delete-window
      :ne "M-W"      #'delete-frame
      :ne "C-M-f"    #'toggle-frame-fullscreen
      :ne "M-n"      #'evil-buffer-new
      :ne "M-N"      #'make-frame
      :ne "M-1"      (λ! (+workspace/switch-to 0))
      :ne "M-2"      (λ! (+workspace/switch-to 1))
      :ne "M-3"      (λ! (+workspace/switch-to 2))
      :ne "M-4"      (λ! (+workspace/switch-to 3))
      :ne "M-5"      (λ! (+workspace/switch-to 4))
      :ne "M-6"      (λ! (+workspace/switch-to 5))
      :ne "M-7"      (λ! (+workspace/switch-to 6))
      :ne "M-8"      (λ! (+workspace/switch-to 7))
      :ne "M-9"      (λ! (+workspace/switch-to 8))
      :ne "M-0"      #'+workspace/switch-to-last

      ;; Other sensible, textmate-esque global bindings
      :ne "M-r"      #'+eval/buffer
      :ne "M-R"      #'+eval/region-and-replace
      :ne "M-b"      #'+default/compile
      :ne "M-a"      #'mark-whole-buffer
      :ne "M-q"      (if (daemonp) #'delete-frame #'evil-quit-all)
      :ne "M-f"      #'swiper
      :m  "A-j"      #'+default:multi-next-line
      :m  "A-k"      #'+default:multi-previous-line
      :nv "C-SPC"    #'+evil:fold-toggle
      :gnvimer "M-v" #'clipboard-yank

      ;; Easier window navigation
      :en "C-h"      #'evil-window-left
      :en "C-j"      #'evil-window-down
      :en "C-k"      #'evil-window-up
      :en "C-l"      #'evil-window-right
      ;; And movement...
      :en "C-S-h"    #'+evil/window-move-left
      :en "C-S-j"    #'+evil/window-move-down
      :en "C-S-k"    #'+evil/window-move-up
      :en "C-S-l"    #'+evil/window-move-right
      ;; And splitting...
      :n "_"         #'evil-window-split
      :n "|"         #'evil-window-vsplit

      ;; Swap ; and :
      :n ";"         #'evil-ex
      :n ":"         #'evil-snipe-repeat
      ;; Swap ' and `, since the harder to reach one is just.. Better!
      :n "`"         #'evil-goto-mark-line
      :n "'"         #'evil-goto-mark

      ;; Make backspace comment out stuff.
      :n "DEL"       #'evilnc-comment-or-uncomment-lines
      :v "DEL"       #'evilnc-comment-or-uncomment-lines

      "C-x p"        #'+popup/other

      ;; --- <leader> -------------------------------------
      (:leader
        :desc "Ex command"                    :nv ";"  #'evil-ex
        :desc "M-x"                           :nv ":"  #'execute-extended-command
        :desc "Pop up scratch buffer"         :nv "x"  #'doom/open-scratch-buffer
        :desc "Org Capture"                   :nv "X"  #'org-capture
        ;:desc "eshell"                        :nv "'"  #'+eshell/open-popup
        :desc "eshell"                        :nv "'"  #'eshell-here

        (:desc "git" :prefix "g"
        ;  :desc "Magit blame"                 :n  "b"   #'magit-blame
        ;  :desc "Magit commit"                :n  "c"   #'magit-commit
        ;  :desc "Magit clone"                 :n  "C"   #'magit-clone
        ;  :desc "Magit dispatch"              :n  "d"   #'magit-dispatch-popup
        ;  :desc "Magit find-file"             :n  "f"   #'magit-find-file
          :desc "Magit status"                :n  "s"   #'magit-status
        ;  :desc "List gists"                  :n  "S"   #'+gist:list
        ;  :desc "Initialize repo"             :n  "i"   #'magit-init
        ;  :desc "Magit buffer log"            :n  "l"   #'magit-log-buffer-file
        ;  :desc "List repositories"           :n  "L"   #'magit-list-repositories
        ;  :desc "Magit push popup"            :n  "p"   #'magit-push-popup
        ;  :desc "Magit pull popup"            :n  "P"   #'magit-pull-popup
        ;  :desc "Git revert hunk"             :n  "r"   #'git-gutter:revert-hunk
        ;  :desc "Git revert file"             :n  "R"   #'vc-revert
        ;  :desc "Git stage hunk"              :n  "h"   #'git-gutter:stage-hunk
        ;  :desc "Git stage file"              :n  "F"   #'magit-stage-file
        ;  :desc "Git time machine"            :n  "t"   #'git-timemachine-toggle
        ;  :desc "Git unstage file"            :n  "U"   #'magit-unstage-file
        ;  :desc "Next hunk"                   :nv "]"   #'git-gutter:next-hunk
        ;  :desc "Previous hunk"               :nv "["   #'git-gutter:previous-hunk)
        )

        ;(:desc "help" :prefix "h"
        ;  :n "h" help-map
        ;  :desc "Apropos"                     :n  "a"   #'apropos
        ;  :desc "Describe char"               :n  "c"   #'describe-char
        ;  :desc "Describe DOOM module"        :n  "d"   #'doom/describe-module
        ;  :desc "Open Doom manual"            :n  "D"   #'doom/help
        ;  :desc "Describe function"           :n  "f"   #'describe-function
        ;  :desc "Describe face"               :n  "F"   #'describe-face
        ;  :desc "Info"                        :n  "i"   #'info-lookup-symbol
        ;  :desc "Describe key"                :n  "k"   #'describe-key
        ;  :desc "Find documentation"          :n  "K"   #'+lookup/documentation
        ;  :desc "Find library"                :n  "l"   #'find-library
        ;  :desc "Command log"                 :n  "L"   #'global-command-log-mode
        ;  :desc "Toggle Emacs log"            :n  "m"   #'view-echo-area-messages
        ;  :desc "Describe mode"               :n  "M"   #'describe-mode
        ;  :desc "Toggle profiler"             :n  "p"   #'doom/toggle-profiler
        ;  :desc "Reload theme"                :n  "R"   #'doom//reload-theme
        ;  :desc "Describe DOOM setting"       :n  "s"   #'doom/describe-setting
        ;  :desc "Describe variable"           :n  "v"   #'describe-variable
        ;  :desc "Describe at point"           :n  "."   #'helpful-at-point
        ;  :desc "What face"                   :n  "'"   #'doom/what-face
        ;  :desc "What minor modes"            :n  ";"   #'doom/what-minor-mode)

        ;(:desc "insert" :prefix "i"
        ;  :desc "From kill-ring"              :nv "y"   #'counsel-yank-pop
        ;  :desc "From evil registers"         :nv "r"   #'counsel-evil-registers
        ;  :desc "From snippet"                :nv "s"   #'yas-insert-snippet)

        ;(:desc "notes" :prefix "n"
        ;  :desc "Find file in notes"          :n  "n"   #'+default/find-in-notes
        ;  :desc "Browse notes"                :n  "N"   #'+default/browse-notes
        ;  :desc "Org capture"                 :n  "x"   #'org-capture)

        (:desc "apps" :prefix "a"
          ;; applications
          :desc "Ranger"                      :n  "r"   #'ranger
        ;  :desc "Elfeed"                      :n  "e"   #'=rss
        ;  :desc "Email"                       :n  "m"   #'=email
        ;  :desc "Twitter"                     :n  "t"   #'=twitter
        ;  :desc "Regex"                       :n  "x"   #'=regex ; TODO: Try this regex thing
          )

        ;(:desc "open" :prefix "o"
        ;  :desc "Default browser"             :n  "b"   #'browse-url-of-file
        ;  :desc "Debugger"                    :n  "d"   #'+debug/open
        ;  :desc "REPL"                        :n  "r"   #'+eval/open-repl
        ;                                      :v  "r"   #'+eval:repl
        ;  ;; :desc "Neotree"                     :n  "n"   #'+neotree/open
        ;  ;; :desc "Neotree: on this file"       :n  "N"   #'+neotree/find-this-file
        ;  :desc "Imenu sidebar"               :nv "i"   #'imenu-list-smart-toggle
        ;  :desc "Eshell"                      :n  "e"   #'+eshell/open
        ;  :desc "Terminal"                    :n  "t"   #'+term/open-popup-in-project

        ;  ;; ;; macos
        ;  ;; (:when IS-MAC
        ;  ;;   :desc "Reveal in Finder"          :n  "o"   #'+macos/reveal-in-finder
        ;  ;;   :desc "Reveal project in Finder"  :n  "O"   #'+macos/reveal-project-in-finder
        ;  ;;   :desc "Send to Transmit"          :n  "u"   #'+macos/send-to-transmit
        ;  ;;   :desc "Send project to Transmit"  :n  "U"   #'+macos/send-project-to-transmit
        ;  ;;   :desc "Send to Launchbar"         :n  "l"   #'+macos/send-to-launchbar
        ;  ;;   :desc "Send project to Launchbar" :n  "L"   #'+macos/send-project-to-launchbar)
        ;  )

        ;(:desc "project" :prefix "p"
        ;  ;:desc "Browse project"          :n  "." #'+default/browse-project
        ;  :desc "Browse project"              :n  "p"   #'+default/browse-project
        ;  :desc "Find file in project"        :n  "/"   #'projectile-find-file
        ;  :desc "Run cmd in project root"     :nv "!"   #'projectile-run-shell-command-in-root
        ;  :desc "Compile project"             :n  "c"   #'projectile-compile-project
        ;  :desc "Find other file"             :n  "o"   #'projectile-find-other-file
        ;  :desc "Switch project"              :n  "p"   #'projectile-switch-project
        ;  :desc "Recent project files"        :n  "r"   #'projectile-recentf
        ;  :desc "List project tasks"          :n  "t"   #'+ivy/tasks
        ;  :desc "Invalidate cache"            :n  "x"   #'projectile-invalidate-cache)

        ;(:desc "quit" :prefix "q"
        ;  :desc "Save and quit"               :n  "q"   #'evil-save-and-quit
        ;  :desc "Quit (forget session)"       :n  "Q"   #'+workspace/kill-session-and-quit
        ;  ;:desc "Restart & restore"           :n  "r"   #'+workspace/restart-emacs-then-restore
        ;  :desc "Restart"                     :n  "R"   #'restart-emacs)

        ;;; (:when (featurep! :tools upload)
        ;;;   (:desc "remote" :prefix "r"
        ;;;     :desc "Upload local"              :n  "u"   #'ssh-deploy-upload-handler
        ;;;     :desc "Upload local (force)"      :n  "U"   #'ssh-deploy-upload-handler-forced
        ;;;     :desc "Download remote"           :n  "d"   #'ssh-deploy-download-handler
        ;;;     :desc "Diff local & remote"       :n  "D"   #'ssh-deploy-diff-handler
        ;;;     :desc "Browse remote files"       :n  "."   #'ssh-deploy-browse-remote-handler
        ;;;     :desc "Detect remote changes"     :n  ">"   #'ssh-deploy-remote-changes-handler))

        ;(:desc "snippets" :prefix "s"
        ;  :desc "New snippet"                 :n  "n"   #'yas-new-snippet
        ;  :desc "Insert snippet"              :nv "i"   #'yas-insert-snippet
        ;  :desc "Find snippet for mode"       :n  "s"   #'yas-visit-snippet-file
        ;  :desc "Find snippet"                :n  "S"   #'+default/find-in-snippets)

        ;(:desc "toggle" :prefix "t"
        ;  :desc "Flyspell"                    :n  "s"   #'flyspell-mode
        ;  :desc "Flycheck"                    :n  "f"   #'flycheck-mode
        ;  :desc "Line numbers"                :n  "l"   #'doom/toggle-line-numbers
        ;  :desc "Frame fullscreen"            :n  "F"   #'toggle-frame-fullscreen
        ;  :desc "Indent guides"               :n  "i"   #'highlight-indentation-mode
        ;  :desc "Indent guides (column)"      :n  "I"   #'highlight-indentation-current-column-mode
        ;  :desc "Impatient mode"              :n  "h"   #'+impatient-mode/toggle
        ;  :desc "Big mode"                    :n  "b"   #'doom-big-font-mode
        ;  :desc "Evil goggles"                :n  "g"   #'+evil-goggles/toggle
        ;  :desc "org-tree-slide mode"         :n  "p"   #'+org-present/start)
      )


      ;; --- Personal vim-esque bindings ------------------
      :nv "K"  #'+lookup/documentation
      :n  "zx" #'kill-this-buffer
      :n  "ZX" #'bury-buffer
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      ;; :n  "]w" #'+workspace/switch-right
      ;; :n  "[w" #'+workspace/switch-left
      ;; :m  "gt" #'+workspace/switch-right
      ;; :m  "gT" #'+workspace/switch-left
      :m  "gd" #'+lookup/definition
      :m  "gD" #'+lookup/references
      :n  "gp" #'+evil/reselect-paste
      :n  "gr" #'+eval:region
      :n  "gR" #'+eval/buffer
      :v  "gR" #'+eval:replace-region
      ;; :m  "gs" #'+default/easymotion  ; lazy-load `evil-easymotion'
      :v  "@"  #'+evil:apply-macro
      :n  "g@" #'+evil:apply-macro
      ;; repeat in visual mode (FIXME buggy)
      :v  "."  #'evil-repeat
      ;; don't leave visual mode after shifting
      :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv

      :nv "C-a"   #'evil-numbers/inc-at-pt
      :nv "C-S-a" #'evil-numbers/dec-at-pt

      ;; --- Plugin bindings ------------------------------
      ;; auto-yasnippet
      :i  [C-tab] #'aya-expand
      :nv [C-tab] #'aya-create

      ;; company-mode (vim-like omnicompletion)
      :i "C-l"  #'+company/complete
      ;; TODO: Work out if this is useful vv.
      ;; (:prefix "C-x"
      ;;   :i "C-l"   #'+company/whole-lines
      ;;   :i "C-k"   #'+company/dict-or-keywords
      ;;   :i "C-f"   #'company-files
      ;;   :i "C-]"   #'company-etags
      ;;   :i "s"     #'company-ispell
      ;;   :i "C-s"   #'company-yasnippet
      ;;   :i "C-o"   #'company-capf
      ;;   :i "C-n"   #'company-dabbrev-code
      ;;   :i "C-p"   #'+company/dabbrev-code-previous)
      (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "C-w"     nil
          "RET"     nil
          [return]  nil
          "C-o"     #'company-search-kill-others
          "C-h"     #'company-show-doc-buffer
          "C-l"     #'company-complete-selection
          "C-j"     #'company-select-next
          "C-k"     #'company-select-previous
          "C-s"     #'company-filter-candidates
          "C-S-s"   #'company-search-candidates
          "C-SPC"   #'company-complete-common
          "TAB"     #'company-complete-selection
          [tab]     #'company-complete-selection
          "S-TAB"   #'company-select-previous
          [backtab] #'company-select-previous)
        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-j"        #'company-search-repeat-forward
          "C-k"        #'company-search-repeat-backward
          "C-s"     (λ! (company-search-abort) (company-filter-candidates))
          [escape]  #'company-search-abort))

      ;; counsel
      ; TODO: This breaks ivy somehow?
      ;(:after counsel
      ;  (:map counsel-ag-map
      ;    [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
      ;    "C-SPC"    #'ivy-call-and-recenter ; preview
      ;    "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

      ;; evil
      (:after evil
        :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
        :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
        :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
        :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
        :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

        ;; (:map evil-window-map ; prefix "C-w"
        ;;   "C-S-w"   #'ace-swap-window
        ;;   ;; Window undo/redo
        ;;   "u"       #'winner-undo
        ;;   "C-u"     #'winner-undo
        ;;   "C-r"     #'winner-redo
        ;;   "o"       #'doom/window-enlargen
        ;;   ;; Delete window
        ;;   "c"       #'+workspace/close-window-or-workspace
        ;;   "C-C"     #'ace-delete-window)

          ;; Fix conflicts with company complete (which is only aparent in haskell buffers for some reason)!
          :i "C-j" nil
          :i "C-k" nil
          :i "C-l" nil
          :i "C-h" nil

          ;; Move linewise over wrapped lines!
          :n "j" #'evil-next-visual-line
          :n "k" #'evil-previous-visual-line

          ;; Allow line up/down in insert mode too.
          ;:i "C-e" evil-scroll-line-down
          ;:i "C-y" evil-scroll-line-up
        )

      ;; evil-exchange
      :n  "gx"  #'evil-exchange

      ;; evil-matchit
      :nv [tab] #'+evil/matchit-or-toggle-fold

      ;; evil-magit
      (:after evil-magit
        :map (magit-status-mode-map magit-revision-mode-map)
        :n "C-j" nil
        :n "C-k" nil)

      ;;; TODO Learn multicursor and multiedit (?)
      ;; ;; evil-mc
      ;; (:prefix "gz"
      ;;   :nv "m" #'evil-mc-make-all-cursors
      ;;   :nv "u" #'evil-mc-undo-all-cursors
      ;;   :nv "z" #'+evil/mc-make-cursor-here
      ;;   :nv "t" #'+evil/mc-toggle-cursors
      ;;   :nv "n" #'evil-mc-make-and-goto-next-cursor
      ;;   :nv "p" #'evil-mc-make-and-goto-prev-cursor
      ;;   :nv "N" #'evil-mc-make-and-goto-last-cursor
      ;;   :nv "P" #'evil-mc-make-and-goto-first-cursor
      ;;   :nv "d" #'evil-mc-make-and-goto-next-match
      ;;   :nv "D" #'evil-mc-make-and-goto-prev-match)
      ;; (:after evil-mc
      ;;   :map evil-mc-key-map
      ;;   :nv "C-n" #'evil-mc-make-and-goto-next-cursor
      ;;   :nv "C-N" #'evil-mc-make-and-goto-last-cursor
      ;;   :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
      ;;   :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

      ;; ;; evil-multiedit
      ;; :v  "R"     #'evil-multiedit-match-all
      ;; :n  "M-d"   #'evil-multiedit-match-symbol-and-next
      ;; :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
      ;; :v  "M-d"   #'evil-multiedit-match-and-next
      ;; :v  "M-D"   #'evil-multiedit-match-and-prev
      ;; :nv "C-M-d" #'evil-multiedit-restore
      ;; (:after evil-multiedit
      ;;   (:map evil-multiedit-state-map
      ;;     "M-d" #'evil-multiedit-match-and-next
      ;;     "M-D" #'evil-multiedit-match-and-prev
      ;;     "RET" #'evil-multiedit-toggle-or-restrict-region)
      ;;   (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
      ;;     "C-n" #'evil-multiedit-next
      ;;     "C-p" #'evil-multiedit-prev))


      ;; emacs/eshell
      ;; (:after eshell
      ;;   :map eshell-mode-map
      ;;   ; Regular window movement..
      ;;   "C-h"      #'evil-window-left
      ;;   "C-j"      #'evil-window-down
      ;;   "C-k"      #'evil-window-up
      ;;   "C-l"      #'evil-window-right
      ;;   ;; (:leader
      ;;   ;;   "'" #'kill-this-buffer
      ;;   ;;   )
      ;;   )



      (:after eshell
        "C-'" #'eshell-here
      ;(set-eshell-alias!
      ;  "q"   "quit-and-close"
      ;  "l"   "ls -l"
      ;  "la"  "ls -la"
      ;  "f"   "find-file $1"
      ;  "d"   "dired $1"
      ;  "gl"  "(call-interactively 'magit-log-current)"
      ;  "gs"  "magit-status"
      ;  "gc"  "magit-commit"
      ;  "rg" "rg --color=always $*"
      ;  )
      )

      ;; evil-snipe
      (:after evil-snipe
        :map evil-snipe-parent-transient-map
        ;; switch to evil-easymotion/avy after a snipe
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively
                   (evilem-create #'evil-snipe-repeat
                                  :bind ((evil-snipe-scope 'whole-buffer)
                                         (evil-snipe-enable-highlight)
                                         (evil-snipe-enable-incremental-highlight))))))

      ;; evil-surround
      :v  "S"  #'evil-surround-region
      ;:o  "s"  #'evil-surround-edit
      :o  "S"  #'evil-surround-edit

      ;; expand-region
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region

      ;; flycheck
      :m  "]e" #'next-error
      :m  "[e" #'previous-error
      (:after flycheck
        :map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)

      ;; flyspell
      :m  "]S" #'flyspell-correct-word-generic
      :m  "[S" #'flyspell-correct-previous-word-generic
      (:after flyspell
        ;; Press RET on misspelled words to correct them
        (:map flyspell-mouse-map
          "RET" #'flyspell-correct-word-generic
          "<mouse-1>" #'flyspell-correct-word-generic))

      ;; git-gutter
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk

      ;; git-timemachine
      (:after git-timemachine
        (:map git-timemachine-mode-map
          :n "C-p" #'git-timemachine-show-previous-revision
          :n "C-n" #'git-timemachine-show-next-revision
          :n "[["  #'git-timemachine-show-previous-revision
          :n "]]"  #'git-timemachine-show-next-revision
          :n "q"   #'git-timemachine-quit
          :n "gb"  #'git-timemachine-blame))

      ;; gist
      (:after gist
        :map gist-list-menu-mode-map
        :n "RET" #'+gist/open-current
        :n "b"   #'gist-browse-current-url
        :n "c"   #'gist-add-buffer
        :n "d"   #'gist-kill-current
        :n "f"   #'gist-fork
        :n "q"   #'quit-window
        :n "r"   #'gist-list-reload
        :n "s"   #'gist-star
        :n "S"   #'gist-unstar
        :n "y"   #'gist-print-current-url)

;      ;; helm
;      (:after helm
;        (:map helm-map
;          "ESC"        nil
;          "C-S-n"      #'helm-next-source
;          "C-S-p"      #'helm-previous-source
;          "C-u"        #'helm-delete-minibuffer-contents
;          "C-w"        #'backward-kill-word
;          "C-r"        #'evil-paste-from-register ; Evil registers in helm! Glorious!
;          "C-b"        #'backward-word
;          [left]       #'backward-char
;          [right]      #'forward-char
;          [escape]     #'helm-keyboard-quit
;          [tab]        #'helm-execute-persistent-action)
;
;        (:after helm-files
;          (:map helm-generic-files-map
;            :e "ESC"     #'helm-keyboard-quit)
;          (:map helm-find-files-map
;            "C-w" #'helm-find-files-up-one-level
;            "TAB" #'helm-execute-persistent-action))
;
;        (:after helm-ag
;          (:map helm-ag-map
;            "<backtab>"  #'helm-ag-edit)))

      ;; hl-todo
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous

      ;; ivy TODO: Learn these binds
      (:after ivy
        :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "C-SPC"  #'ivy-call-and-recenter
        ;"M-z"    #'undo
        ;"M-v"    #'yank
        "C-y"    #'yank
        "C-r"    #'evil-paste-from-register
        "C-k"    #'ivy-previous-line
        "C-j"    #'ivy-next-line
        "C-A-k"  #'ivy-scroll-down-command
        "C-A-j"  #'ivy-scroll-up-command
        "C-l"    #'ivy-alt-done
        "C-w"    #'ivy-backward-kill-word
        "C-h"    #'ivy-backward-kill-word
        "C-u"    #'ivy-kill-line
        "C-b"    #'backward-word
        "C-f"    #'forward-word)

      ;; neotree
      (:after neotree
        :map neotree-mode-map
        :n "g"         nil
        :n [tab]       #'neotree-quick-look
        :n "RET"       #'neotree-enter
        :n [backspace] #'evil-window-prev
        :n "c"         #'neotree-create-node
        :n "r"         #'neotree-rename-node
        :n "d"         #'neotree-delete-node
        :n "j"         #'neotree-next-line
        :n "k"         #'neotree-previous-line
        :n "n"         #'neotree-next-line
        :n "p"         #'neotree-previous-line
        :n "h"         #'+neotree/collapse-or-up
        :n "l"         #'+neotree/expand-or-open
        :n "J"         #'neotree-select-next-sibling-node
        :n "K"         #'neotree-select-previous-sibling-node
        :n "H"         #'neotree-select-up-node
        :n "L"         #'neotree-select-down-node
        :n "G"         #'evil-goto-line
        :n "gg"        #'evil-goto-first-line
        :n "v"         #'neotree-enter-vertical-split
        :n "s"         #'neotree-enter-horizontal-split
        :n "q"         #'neotree-hide
        :n "R"         #'neotree-refresh)

      (:after evil-org
        :map evil-org-mode-map
        :n "M-TAB" #'org-global-cycle
        :localleader
        :n "l" #'org-toggle-latex-fragment
        :n "RET" #'org-toggle-latex-fragment
        :n "r" #'my-execute-babel
        ;; )
        :n "t" #'org-todo
        (:desc "clock" :prefix "c"
          :n "c" #'org-clock-in
          :n "C" #'org-clock-out
          :n "g" #'org-clock-goto
          :n "G" (λ! (org-clock-goto 'select))
          :n "x" #'org-clock-cancel))

      ;; realgud
      (:after realgud
        :map realgud:shortkey-mode-map
        :n "j" #'evil-next-line
        :n "k" #'evil-previous-line
        :n "h" #'evil-backward-char
        :n "l" #'evil-forward-char
        :m "n" #'realgud:cmd-next
        :m "b" #'realgud:cmd-break
        :m "B" #'realgud:cmd-clear
        :n "c" #'realgud:cmd-continue)

      ;; rotate-text
      :n  "!"  #'rotate-text

      ;; smart-forward
      :m  "g]" #'smart-forward
      :m  "g[" #'smart-backward

      ;; swiper
      (:after swiper
        (:map swiper-map
          [backtab]  #'+ivy/wgrep-occur))

      ;; undo-tree -- undo/redo for visual regions
      ;; :v "C-u" #'undo-tree-undo
      ;; :v "C-r" #'undo-tree-redo

      ;; yasnippet
      (:after yasnippet
        (:map yas-keymap
          "C-e"           #'+snippets/goto-end-of-field
          "C-a"           #'+snippets/goto-start-of-field
          ;"<M-right>"     #'+snippets/goto-end-of-field
          ;"<M-left>"      #'+snippets/goto-start-of-field
          ;"<M-backspace>" #'+snippets/delete-to-start-of-field
          [backspace]     #'+snippets/delete-backward-char
          [delete]        #'+snippets/delete-forward-char-or-field)
        (:map yas-minor-mode-map
          :ig "<tab>" yas-maybe-expand
          :v  "<tab>" #'yas-insert-snippet
          :ig "TAB" yas-maybe-expand
          :v  "TAB" #'yas-insert-snippet))

      (:after markdown-mode
        (:map markdown-mode-map
          ;; fix conflicts with private bindings
          "<backspace>" nil
          "<M-left>"    nil
          "<M-right>"   nil))


      ;; --- Built-in plugins -----------------------------
      (:after comint
        ;; TAB auto-completion in term buffers
        :map comint-mode-map [tab] #'company-complete)

      (:after help-mode
        :map help-mode-map
        "o" #'ace-link-help
        ">" #'help-go-forward
        "<" #'help-go-back
        "n" #'forward-button
        "p" #'backward-button)

      (:after vc-annotate
        :map vc-annotate-mode-map
        [remap quit-window] #'kill-this-buffer))


;; C-sharp test
(map!
 (:after omnisharp
   :map omnisharp-mode-map
   (:localleader
     :n "d" #'omnisharp-go-to-definition
     :n "D" #'omnisharp-go-to-definition-other-window
     :n "R" #'omnisharp-run-code-action-refactoring
     :n "r" #'omnisharp-rename
     :n "i" #'omnisharp-current-type-information
     :n "I" #'omnisharp-current-type-documentation
     )))

;;
;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.
(map! (:map input-decode-map
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      ;; Restore common editing keys (and ESC) in minibuffer
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             read-expression-map)
        [escape] #'abort-recursive-edit
        (:when (featurep 'evil)
          "C-r" #'evil-paste-from-register)
        "C-a" #'move-beginning-of-line
        "C-w" #'backward-kill-word
        "C-u" #'backward-kill-sentence
        "C-b" #'backward-word
        "C-f" #'forward-word
        "C-z" (λ! (ignore-errors (call-interactively #'undo))))

      (:after evil
        (:map evil-ex-completion-map
          "C-a" #'move-beginning-of-line
          "C-b" #'backward-word
          "C-f" #'forward-word))

      (:after tabulated-list
        (:map tabulated-list-mode-map
          "q" #'quit-window))

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))

;;
;; Evil-collection fixes
;;

(when (featurep 'evil-collection)
  ;; don't interfere with leader key
  (evil-define-key* '(normal visual) special-mode-map (kbd doom-leader-key) nil)
  (after! dired
    (evil-define-key* 'normal dired-mode-map (kbd doom-leader-key) nil))

  ;; don't remap gd or K; Doom does this already
  ;; TODO find a better way
  (after! compile
    (evil-define-key* '(normal visual) compilation-mode-map "gd" nil "K" nil))
  (after! racer
    (evil-define-key* 'normal racer-mode-map "gd" nil "K" nil))
  (after! anaconda-mode
    (evil-define-key* 'normal anaconda-mode-map "gd" nil "K" nil))
  (after! alchemist
    (evil-define-key* 'normal alchemist-mode-map "gd" nil "K" nil "gz" nil))
  (after! go-mode
    (evil-define-key* 'normal go-mode-map "gd" nil "K" nil))
  (after! lua-mode
    (evil-define-key* 'normal lua-mode-map "K" nil)))


(setq doom-localleader-key ",")
