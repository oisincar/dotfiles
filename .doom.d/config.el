;;; private/default/config.el -*- lexical-binding: t; -*-

(load! "bindings")

(setq doom-font (font-spec :family "Source Code Pro" :size 18)) ;18

(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args "--simple-prompt --pprint"
 org-startup-with-inline-images t)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(use-package pyvenv)

;; Make images fill width in org mode by default..
;; (setq org-image-actual-width 1000)

(setq doom-font (font-spec :family "Source Code Pro" :size 18))

;; Make search work using silver-searcher on windows.
;; (Not updating value for some reason??)
(setq counsel-ag-base-command "ag -zS --vimgrep --nocolor --nogroup %s")

;; (setq visual-line-mode t)
