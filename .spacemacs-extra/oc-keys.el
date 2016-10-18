
(setq-default

 evil-escape-key-sequence "qj"
 evil-escape-delay 0.2)

;; Split window vertically using | and horizontally with _
(define-key evil-normal-state-map "|" 'split-window-right-and-focus)
(define-key evil-normal-state-map "_" 'split-window-below-and-focus)

;; swap ; and :
(define-key evil-normal-state-map ":" 'evil-repeat-find-char)
(define-key evil-normal-state-map ";" 'evil-ex)

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



(provide 'oc-keys)
