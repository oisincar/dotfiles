;;; private/default/config.el -*- lexical-binding: t; -*-

(load! "bindings")
;(load! bindings)
(setq doom-font (font-spec :family "Source Code Pro" :size 26)) ;18

;; (require 'ob-ipython)
;; ;; (setq ob-ipython-command "ipython3")
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args "--simple-prompt --pprint"
 org-startup-with-inline-images t)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; (package! pyvenv)
(use-package pyvenv)

;; Make images fill width in org mode by default..
;; (setq org-image-actual-width 1000)
