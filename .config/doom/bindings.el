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

      ;; --- <leader> -------------------------------------
      (:leader
        :desc "eshell"                        :nv "'"  #'+eshell/toggle

        (:desc "git" :prefix "g"
          :desc "Magit status"                :n  "s"   #'magit-status
        )

        ;; (:desc "apps" :prefix "a"
        ;;   :desc "Ranger"                      :n  "r"   #'ranger
        ;; ;  :desc "Regex"                       :n  "x"   #'=regex ; TODO: Try this regex thing
        ;;   )
      )

      :nv "C-a"   #'evil-numbers/inc-at-pt
      :nv "C-S-a" #'evil-numbers/dec-at-pt

      ;; --- Plugin bindings ------------------------------
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

          ;; Fix conflicts with company complete (which is only aparent in haskell buffers for some reason)!
          :i "C-j" nil
          :i "C-k" nil
          :i "C-l" nil
          :i "C-h" nil

          ;; Move linewise over wrapped lines!
          :n "j" #'evil-next-visual-line
          :n "k" #'evil-previous-visual-line

          ;; Allow line up/down in insert mode too.
          :i "C-e" #'evil-scroll-line-down
          :i "C-y" #'evil-scroll-line-up
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

      ;; hl-todo
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous

      ;; vertico
      (:after vertico
       :map vertico-map
       ;; "C-SPC" #'+vertico/embark-preview
       ;; "C-j"   #'vertico-next
       ;; "C-M-j" #'vertico-next-group
       ;; "C-k"   #'vertico-previous
       ;; "C-M-k" #'vertico-previous-group
       ;; "C-h" (cmds! (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
       ;; "C-l" (cmds! (eq 'file (vertico--metadata-get 'category)) #'+vertico/enter-or-preview))
       ; (custom) Enter folder or select file.
       "C-l" (cmds! (eq 'file (vertico--metadata-get 'category)) #'+vertico/enter-or-close)
       "C-p" #'+vertico/embark-preview
       )
      ;; ivy
      ;; (:after ivy
      ;;   :map ivy-minibuffer-map
      ;;   [escape] #'keyboard-escape-quit
      ;;   "C-SPC"  #'ivy-call-and-recenter
      ;;   ;"M-z"    #'undo
      ;;   ;"M-v"    #'yank
      ;;   "C-y"    #'yank
      ;;   "C-r"    #'evil-paste-from-register
      ;;   "C-k"    #'ivy-previous-line
      ;;   "C-j"    #'ivy-next-line
      ;;   "C-A-k"  #'ivy-scroll-down-command
      ;;   "C-A-j"  #'ivy-scroll-up-command
      ;;   "C-l"    #'ivy-alt-done
      ;;   "C-w"    #'ivy-backward-kill-word
      ;;   "C-h"    #'ivy-backward-kill-word
      ;;   "C-u"    #'ivy-kill-line
      ;;   "C-b"    #'backward-word
      ;;   "C-f"    #'forward-word)

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


      (:after eshell
       :map eshell-mode-map
       :n "C-k"  #'evil-window-up
       ; Quit w/ esc.
       :n [escape] #'+eshell/toggle
       )
      )


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
