;;; private/default/config.el -*- lexical-binding: t; -*-

(load! "bindings")

(setq doom-font (font-spec :family "Source Code Pro" :size 26))
;; (setq doom-font (font-spec :family "Source Code Pro" :size 21))
;; (setq doom-font (font-spec :family "Source Code Pro" :size 18))

; Unused ibpython config.. Replaced by using EIN now.
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args "--simple-prompt --pprint"
 org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
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
