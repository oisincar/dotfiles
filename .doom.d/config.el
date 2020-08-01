;;; private/default/config.el -*- lexical-binding: t; -*-

(load! "bindings")

(x-display-pixel-width)

;; Pick font size based on multimonitor/ laptop only/ external big monitor.
(when window-system
  (if (> (list-length (display-monitor-attributes-list)) 1) ; Multi monitor setup...
      (setq doom-font (font-spec :family "Source Code Pro" :size 18))
    (if (> (x-display-pixel-width) 3200) ; width of laptop screen
        (setq doom-font (font-spec :family "Source Code Pro" :size 18)) ; or 21
      (setq doom-font (font-spec :family "Source Code Pro" :size 26))
      )))

(setq-default doom-big-font (font-spec :family "Source Code Pro" :size 34)

              ;; Only replace charachters/ auto-format in some modes.
              +pretty-code-enabled-modes '(emacs-lisp-mode org-mode)
              +format-on-save-enabled-modes '(not emacs-lisp-mode))

; Add habit, TODO: Use.
(add-to-list 'org-modules 'org-habit t)

; Unused ibpython config.. Replaced by using EIN now.
;; (setq
;;  python-shell-interpreter "ipython3"
;;  python-shell-interpreter-args "--simple-prompt --pprint"
;;  org-startup-with-inline-images t)
;; (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;; (use-package pyvenv)
;; Make images fill width in org mode by default..

;; Make search work using silver-searcher on windows.
;; (Not updating value for some reason??)
(setq counsel-ag-base-command "ag -zS --vimgrep --nocolor --nogroup %s")

; Add movement binds for EIN.
(add-hook 'ein:notebook-multilang-mode-hook
          (lambda () (local-set-key (kbd "M-J") #'ein:worksheet-goto-next-input)))
(add-hook 'ein:notebook-multilang-mode-hook
          (lambda () (local-set-key (kbd "M-K") #'ein:worksheet-goto-prev-input)))

(after! org
  ; Set scale for latex fragments to be displayed in org mode.
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
  ; Thicc images
  ;; (setq org-image-actual-width 1000)
  ; Show images on startup
  (setq org-startup-with-inline-images t))

;; Tramp.
(require 'tramp)
(setq tramp-default-method "ssh")
;; TODO: Should be vv, need to test.
;; (after! tramp
;;   (setq tramp-default-method "ssh"))

(defun my-write-mode ()
  (interactive)
  (visual-line-mode)
  (auto-fill-mode))

; TODO: Move to some other (functions?) file
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(defun my-execute-babel ()
  (interactive)
  (org-ctrl-c-ctrl-c)
  (org-display-inline-images))


(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         ;; (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    ;; (split-window-vertically (- height))
    ;; (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

;; emacs/eshell
(after! eshell
  (set-eshell-alias!
   "f"   "find-file $1" ;; TODO: Use, this is pretty neat!
   "l"   "ls -lh"
   "d"   "dired $1"
   "gl"  "(call-interactively 'magit-log-current)"
   "gs"  "magit-status"
   "gc"  "magit-commit"
   "rg"  "rg --color=always $*"))

;; Set search engine to use grep by default, since it's on all servers.
;; (setq +ivy-project-search-engines '(grep rg ag pt))

;; Don't show unity's stupid .meta files, or cargo lock files.
;; TEMP: Can't figure out how to ignore both... Just ignore cargo durr.
(after! counsel
  (setq counsel-find-file-ignore-regexp  "\\.lock\\'")) ;"\\.meta\\'"
;; Don't show stupid ./ and ../ options.
(setq ivy-extra-directories ())

;; Add inline-js as an org language - which exports javascript directly into the file.
(after! org
  (add-to-list 'org-src-lang-modes '("inline-js" . javascript)) ;; js2 if you're fancy

  (setq indent-tabs-mode nil)
  (setq org-src-preserve-indentation t)
;; (defvar org-babel-default-header-args:inline-js
;;   '((:results . "html")
;;     (:exports . "results")))
;; (defun org-babel-execute:inline-js (body _params)
;;   (format "<script type=\"text/javascript\">\n%s\n</script>" body))
  )


;; Whitespace mode - highlight lines that are too long.
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 100) ;; limit line length
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Org mode text wrap rather than inserting newlines
;; (with-eval-after-load 'visual-fill-column
;;   (add-hook 'visual-fill-column-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
;;   (setq split-window-preferred-function 'visual-fill-column-split-window-sensibly)
;;   (advice-add 'text-scale-adjust :after 'visual-fill-column-adjust))
;; (add-hook 'org-mode-hook '(lambda ()
;;                             (auto-fill-mode) ;; Disable automatic insertion of newlines
;;                             ;; make the lines in the buffer wrap around the edges of the screen.
;;                             ;; to press C-c q  or fill-paragraph ever again!
;;                             (visual-line-mode)
;;                             (org-indent-mode)))

;(add-hook '+write-mode-hook
;          (auto-fill-mode) ;; Disable auto insertion of newlines
;          )

;; Refrence/citation stuff.
;; Bibtex now in dissertation writing folder...
(setq bibtex-completion-bibliography '( "~/Devcrap/College5th/Dissertation/DISS-Message-Passing-NNs/Writings/Dissertation/refs.bib") ;the major bibtex file
      bibtex-completion-library-path "~/Devcrap/bibliography/reference/pdf/" ;the directory to store pdfs
      bibtex-completion-notes-path "~/Devcrap/bibliography/ref.org" ;the note file for reference notes
      ;; org-directory "~/Dropbox/org"
      ;; ~/Devcrap/bibliography/reference/BibDissert.bib
      org-ref-default-bibliography '( "~/Devcrap/College5th/Dissertation/DISS-Message-Passing-NNs/Writings/Dissertation/refs.bib" )
      org-ref-bibliography-notes "~/Devcrap/bibliography/ref.org"
      org-ref-pdf-directory "~/Devcrap/bibliography/reference/pdf/"
      )
;; Latex export with stuff better.
;; (setq org-latex-pdf-process (list
   ;; "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
(setq org-latex-pdf-process (list "latexmk -pdf %f"))
(setq org-latex-caption-above nil)    ; Caption below everything, including tables.
(setq org-export-with-smart-quotes t) ; Nice looking quotes

;; Add acm template to latex
(after! ox-latex
  (add-to-list 'org-latex-classes
               '("acmart"
                 "\\documentclass{acmart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; Trying the blind interface!
;; (setq
;;  '(emacspeak-character-echo nil)
;;  '(emacspeak-line-echo t)
;;  '(emacspeak-word-echo nil)
;;  '(espeak-default-speech-rate 250)
;;  '(line-number-mode nil))
;; (dtk-toggle-split-caps t)
;; (dtk-toggle-allcaps-beep t)

;; (require 'emacspeak-ivy)


;; NOTE: When installing, in order to use nikola with code export...
;; must mx/package-install htmlize. EVEN THOUGH it's required, has to be
;; accessable outside doom.

;; Rust config...
(setq rustic-lsp-server 'rust-analyzer)

;; Make surround only add spaces on closing brackets.
(after! evil-surround
  (evil-add-to-alist
    'evil-surround-pairs-alist
    ; ?\( '("(" . ")")
    ?\( '("(" . ")")
    ?\[ '("[" . "]")
    ?\{ '("{" . "}")
    ?\) '("( " . " )")
    ?\] '("[ " . " ]")
    ?\} '("{ " . " }")))
