;;; private/default/config.el -*- lexical-binding: t; -*-

(load! "bindings")

(when window-system
  (if (> (x-display-pixel-width) 3200) ; width of laptop screen
      (setq doom-font (font-spec :family "Source Code Pro" :size 18)) ; or 21
      (setq doom-font (font-spec :family "Source Code Pro" :size 26))
    ))

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
(setq org-image-actual-width 1000)

;; Make search work using silver-searcher on windows.
;; (Not updating value for some reason??)
(setq counsel-ag-base-command "ag -zS --vimgrep --nocolor --nogroup %s")

; Add movement binds for EIN.
(add-hook 'ein:notebook-multilang-mode-hook
          (lambda () (local-set-key (kbd "M-J") #'ein:worksheet-goto-next-input)))
(add-hook 'ein:notebook-multilang-mode-hook
          (lambda () (local-set-key (kbd "M-K") #'ein:worksheet-goto-prev-input)))

; Set scale for latex fragments to be displayed in org mode.
(after! org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5)))

;; Tramp.
;; (setq tramp-default-method "ssh")

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
   "f"   "find-file $1"
   "l"   "ls -lh"
   "d"   "dired $1"
   "gl"  "(call-interactively 'magit-log-current)"
   "gs"  "magit-status"
   "gc"  "magit-commit"
   "rg"  "rg --color=always $*"))
